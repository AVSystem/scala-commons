package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.TypeClassDerivation

import scala.collection.mutable
import scala.reflect.macros.blackbox

class GenCodecMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) with TypeClassDerivation {

  import c.universe._

  override def allowOptionalParams: Boolean = true

  def mkTupleCodec[T: WeakTypeTag](elementCodecs: Tree*): Tree = instrument {
    val tupleTpe = weakTypeOf[T]
    val indices = elementCodecs.indices
    q"""
      new $GenCodecObj.ListCodec[$tupleTpe] {
        def nullable = true
        def readList(input: $SerializationPkg.ListInput) =
          (..${indices.map(i => q"${elementCodecs(i)}.read(input.nextElement())")})
        def writeList(output: $SerializationPkg.ListOutput, value: $tupleTpe) = {
          output.declareSize(${elementCodecs.size})
          ..${indices.map(i => q"${elementCodecs(i)}.write(output.writeElement(), value.${tupleGet(i)})")}
        }
      }
     """
  }

  // GenCodec or GenObjectCodec
  private val CodecObj: ModuleSymbol = c.prefix.actualType.termSymbol.asModule
  private val CodecCls: ClassSymbol = CodecObj.companion.asClass

  def implementDeferredInstance(tpe: Type): Tree = q"new $CodecObj.Deferred[$tpe]"

  override def typeClassInstance(tpe: Type): Type =
    getType(tq"$CodecCls[$tpe]")

  override def dependencyType(tpe: Type): Type =
    getType(tq"$GenCodecCls[$tpe]")

  override def dependency(depTpe: Type, tcTpe: Type, param: Symbol): Tree = {
    val clue = s"Cannot materialize $tcTpe because of problem with parameter ${param.name}:\n"
    val depTcTpe = dependencyType(depTpe)
    inferCachedImplicit(depTcTpe, ErrorCtx(clue, param.pos)).reference(Nil)
  }

  override def materializeFor(tpe: Type): Tree = {
    val tsym = tpe.dealias.typeSymbol
    if (isSealedHierarchyRoot(tsym)) findAnnotation(tsym, FlattenAnnotType) match {
      case Some(annot) =>
        val caseFieldName = annot.tree match {
          case Apply(_, Nil) => DefaultCaseField
          case Apply(_, StringLiteral(str) :: _) => str
          case Apply(_, arg :: _) => c.abort(arg.pos, s"String literal expected as case field name")
        }
        flatForSealedHierarchy(tpe, caseFieldName)
      case None =>
        super.materializeFor(tpe)
    } else
      super.materializeFor(tpe)
  }

  private def newDepName(sym: Symbol): TermName =
    newDepName(sym.name.toString)

  private def newDepName(base: String): TermName =
    c.freshName(TermName(base + "Codec"))

  private def depNamesMap(syms: Iterable[Symbol]) =
    syms.iterator.map(s => (s, newDepName(s))).toMap

  private def mkParamLessCall(prefix: Tree, sym: Symbol): Tree = {
    def addParams(tree: Tree, paramLists: List[List[Symbol]]): Tree = paramLists match {
      case Nil => tree
      case last :: Nil if last.exists(_.isImplicit) => tree
      case _ :: tail => addParams(q"$tree()", tail)
    }

    addParams(q"$prefix.$sym", sym.typeSignature.paramLists)
  }

  private def mkArray[T: Liftable](elemTpe: Tree, elems: Seq[T]): Tree =
    q"""
      val res = new $ScalaPkg.Array[$elemTpe](${elems.size})
      ..${elems.zipWithIndex.map({ case (e, i) => q"res($i) = $e" })}
      res
     """

  private def generatedMembers(tpe: Type): List[(Symbol, Type)] =
    tpe.members.filter(isGenerated).map { sym =>
      val sig = sym.typeSignatureIn(tpe)
      if (sig.typeParams.nonEmpty) {
        abort(s"Generated member ${sym.name} of $tpe must not take type parameters")
      }
      if (!sig.paramLists.forall(_.forall(p => p.isImplicit || p.asTerm.isParamWithDefault))) {
        abort(s"Generated member ${sym.name} of $tpe must not take parameters unless they're implicit or have a default value")
      }
      (sym, sig.finalResultType)
    }.toList

  def forSingleton(tpe: Type, singleValueTree: Tree): Tree = {
    val generated = generatedMembers(tpe)
    def safeSingleValue: Tree = typecheck(singleValueTree)

    if (generated.isEmpty)
      q"new $SerializationPkg.SingletonCodec[$tpe](${tpe.toString}, $safeSingleValue)"
    else {
      val tcTpe = typeClassInstance(tpe)
      val targetNames = targetNameMap(generated.map(_._1))
      val depNames = depNamesMap(generated.map(_._1))

      def generatedDepDeclaration(sym: Symbol, depTpe: Type): Tree =
        q"lazy val ${depNames(sym)} = ${super.dependency(depTpe, tcTpe, sym)}"

      def generatedWrite(sym: Symbol): Tree =
        q"writeField(${targetNames(sym)}, output, ${mkParamLessCall(q"value", sym)}, ${depNames(sym)})"

      q"""
        new $SerializationPkg.SingletonCodec[$tpe](${tpe.toString}, $safeSingleValue) {
          ..${generated.map({ case (sym, depTpe) => generatedDepDeclaration(sym, depTpe) })}
          override def size(value: $tpe): $IntCls = ${generated.size}
          override def writeFields(output: $SerializationPkg.ObjectOutput, value: $tpe): $UnitCls = {
            ..${generated.map({ case (sym, _) => generatedWrite(sym) })}
          }
        }
      """
    }
  }

  def isTransientDefault(param: ApplyParam): Boolean =
    param.defaultValue.nonEmpty && hasAnnotation(param.sym, TransientDefaultAnnotType)

  def isOptimizedPrimitive(param: ApplyParam): Boolean = {
    val vt = param.valueType
    vt =:= typeOf[Boolean] || vt =:= typeOf[Int] || vt =:= typeOf[Long] || vt =:= typeOf[Double]
  }

  def isOutOfOrder(sym: Symbol): Boolean =
    hasAnnotation(sym, OutOfOrderAnnotType)

  def forApplyUnapply(applyUnapply: ApplyUnapply, applyParams: List[ApplyParam]): Tree = {
    val ApplyUnapply(tpe, companion, _, unapply, _) = applyUnapply
    val params = applyParams.map { p =>
      findAnnotation(p.sym, WhenAbsentAnnotType, tpe).fold(p) { annot =>
        val newDefault = annot.tree.children.tail.head
        if (!(newDefault.tpe <:< p.valueType)) {
          abortAt(s"expected value of type ${p.valueType} in @whenAbsent annotation, got ${newDefault.tpe.widen}", p.sym.pos)
        }
        p.copy(defaultValue = c.untypecheck(newDefault))
      }
    }

    val dtpe = tpe.dealias
    val tcTpe = typeClassInstance(dtpe)
    val generated = generatedMembers(dtpe)
    val nameBySym = targetNameMap(params.map(_.sym) ++ generated.map(_._1))

    val genDepNames = generated.map({ case (sym, _) => (sym, newDepName(sym)) }).toMap

    def generatedDepDeclaration(sym: Symbol, depTpe: Type) =
      q"lazy val ${genDepNames(sym)} = ${super.dependency(depTpe, tcTpe, sym)}"

    // don't use apply/unapply when they're synthetic (for case class) to avoid reference to companion object

    val ts = dtpe.typeSymbol
    val caseClass = ts.isClass && ts.asClass.isCaseClass && companion.symbol == ts.companion
    val canUseFields = caseClass && (unapply == NoSymbol || unapply.isSynthetic) && params.forall { p =>
      alternatives(dtpe.member(p.sym.name)).exists { f =>
        f.isTerm && f.asTerm.isCaseAccessor && f.isPublic && f.typeSignatureIn(dtpe).finalResultType =:= p.valueType
      }
    }

    def writeField(p: ApplyParam, value: Tree): Tree = {
      val transientValue =
        if (isTransientDefault(p)) Some(p.defaultValue)
        else p.optionLike.map(ol => q"${ol.reference(Nil)}.none")

      val writeArgsNoTransient = q"output" :: q"${p.idx}" :: List(value)
      val writeArgs = writeArgsNoTransient ::: transientValue.toList
      val writeTargs = if (isOptimizedPrimitive(p)) Nil else List(p.valueType)
      q"""
        if (ignoreTransientDefault)
          writeField[..$writeTargs](..$writeArgsNoTransient)
        else
          writeField[..$writeTargs](..$writeArgs)
       """
    }

    def ignoreTransientDefaultCheck: Tree =
      q"val ignoreTransientDefault = output.customEvent($SerializationPkg.IgnoreTransientDefaultMarker, ())"

    def writeFields: Tree = params match {
      case Nil =>
        if (canUseFields)
          q"()"
        else
          q"""
            if(!$companion.$unapply[..${dtpe.typeArgs}](value)) {
              unapplyFailed
            }
           """
      case List(p: ApplyParam) =>
        if (canUseFields)
          q"""
            $ignoreTransientDefaultCheck
            ${writeField(p, q"value.${p.sym.name}")}
           """
        else
          q"""
            val unapplyRes = $companion.$unapply[..${dtpe.typeArgs}](value)
            if (unapplyRes.isEmpty) unapplyFailed
            else {
              $ignoreTransientDefaultCheck
              ${writeField(p, q"unapplyRes.get")}
            }
           """
      case _ =>
        if (canUseFields)
          q"""
            $ignoreTransientDefaultCheck
            ..${params.map(p => writeField(p, q"value.${p.sym.name}"))}
           """
        else
          q"""
            val unapplyRes = $companion.$unapply[..${dtpe.typeArgs}](value)
            if (unapplyRes.isEmpty) unapplyFailed
            else {
              val t = unapplyRes.get
              $ignoreTransientDefaultCheck
              ..${params.map(p => writeField(p, q"t.${tupleGet(p.idx)}"))}
            }
           """
    }

    def mayBeTransient(p: ApplyParam): Boolean =
      p.optionLike.nonEmpty || isTransientDefault(p)

    def transientValue(p: ApplyParam): Tree = p.optionLike match {
      case Some(optionLike) => q"${optionLike.reference(Nil)}.none"
      case None => p.defaultValue
    }

    def countTransientFields: Tree =
      if (canUseFields)
        params.filter(mayBeTransient).foldLeft[Tree](q"0") {
          (acc, p) => q"$acc + (if(value.${p.sym.name} == ${transientValue(p)}) 1 else 0)"
        }
      else if (!params.exists(mayBeTransient)) q"0"
      else params match {
        case List(p: ApplyParam) =>
          q"""
            val unapplyRes = $companion.$unapply[..${dtpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else if(unapplyRes.get == ${transientValue(p)}) 1 else 0
          """
        case _ =>
          val res = params.filter(mayBeTransient).foldLeft[Tree](q"0") {
            (acc, p) => q"$acc + (if(t.${tupleGet(p.idx)} == ${transientValue(p)}) 1 else 0)"
          }
          q"""
            val unapplyRes = $companion.$unapply[..${dtpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else {
              val t = unapplyRes.get
              $res
            }
           """
      }

    if (isTransparent(dtpe.typeSymbol)) params match {
      case List(p: ApplyParam) =>
        if (generated.nonEmpty) {
          abort(s"class marked as @transparent cannot have @generated members")
        }

        val unwrapBody = if (canUseFields)
          q"value.${p.sym.name}"
        else
          q"""
            val unapplyRes = $companion.$unapply[..${dtpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else unapplyRes.get
           """

        q"""
          new $CodecObj.Transformed[$dtpe,${p.valueType}](
            $CodecObj.makeLazy($CodecObj[${p.valueType}]),
            value => $unwrapBody,
            underlying => ${applyUnapply.mkApply(List(q"underlying"))}
          )
         """
      case _ =>
        abort(s"@transparent annotation found on class with ${params.size} parameters, expected exactly one.")

    } else {

      def readField(param: ApplyParam): Tree = {
        val defValue = Option(param.defaultValue).filter(_ != EmptyTree) orElse
          param.optionLike.map(ol => q"${ol.reference(Nil)}.none")
        defValue match {
          case None => q"getField[${param.valueType}](fieldValues, ${param.idx})"
          case Some(tree) => q"getField[${param.valueType}](fieldValues, ${param.idx}, $tree)"
        }
      }

      def generatedWrite(sym: Symbol): Tree =
        q"writeField(${nameBySym(sym)}, output, ${mkParamLessCall(q"value", sym)}, ${genDepNames(sym)})"

      val useProductCodec = canUseFields && generated.isEmpty && !params.exists(mayBeTransient) &&
        (isScalaJs || !params.exists(isOptimizedPrimitive))

      val baseClass = TypeName(if (useProductCodec) "ProductCodec" else "ApplyUnapplyCodec")

      def sizeMethod: List[Tree] = if (useProductCodec) Nil else {
        val res =
          q"""
            def size(value: $dtpe): $IntCls =
              ${params.size} + ${generated.size} - $countTransientFields
          """
        List(res)
      }

      def writeMethod: Tree = if (useProductCodec) q"()" else
        q"""
          def writeFields(output: $SerializationPkg.ObjectOutput, value: $dtpe) = {
            $writeFields
            ..${generated.map({ case (sym, _) => generatedWrite(sym) })}
          }
         """

      val allOptionLikes = params.flatMap(_.optionLike).toSet
      val optionLikeDecls = allOptionLikes.collect {
        case ii: InferredImplicit => mkPrivateLazyValOrDef(ii)
      }

      def fieldCodec(param: ApplyParam): Tree =
        param.optionLike.fold(param.instance) { ol =>
          q"new $SerializationPkg.OptionalFieldValueCodec(${ol.reference(Nil)}, ${param.instance})"
        }

      q"""
        new $SerializationPkg.$baseClass[$dtpe](
          ${dtpe.toString},
          ${typeOf[Null] <:< dtpe},
          ${mkArray(StringCls, params.map(p => nameBySym(p.sym)))}
        ) {
          ..$optionLikeDecls
          def dependencies = {
            ..${cachedImplicitDeclarations(ci => if (!allOptionLikes.contains(ci)) q"val ${ci.name} = ${ci.body}" else q"()")}
            ${mkArray(tq"$GenCodecCls[_]", params.map(fieldCodec))}
          }
          ..${generated.collect({ case (sym, depTpe) => generatedDepDeclaration(sym, depTpe) })}
          def instantiate(fieldValues: $SerializationPkg.FieldValues) =
            ${applyUnapply.mkApply(params.map(p => p.asArgument(readField(p))))}
          ..$sizeMethod
          $writeMethod
        }
       """
    }
  }

  private def targetNameMap(symbols: Seq[Symbol]): Map[Symbol, String] = {
    val paramsOrSubclasses =
      if (symbols.exists(_.isClass)) "Subclasses"
      else "Parameters or members"
    symbols.groupBy(st => targetName(st)).map {
      case (targetName, List(sym)) => (sym, targetName)
      case (targetName, syms) =>
        abort(s"$paramsOrSubclasses ${syms.map(_.name).mkString(", ")} have the same @name: $targetName")
    }
  }

  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree = {
    val targetNameBySym = targetNameMap(subtypes.map(_.sym))
    q"""
      new $SerializationPkg.NestedSealedHierarchyCodec[$tpe](
        ${tpe.toString},
        ${typeOf[Null] <:< tpe},
        ${mkArray(StringCls, subtypes.map(st => targetNameBySym(st.sym)))},
        ${mkArray(tq"$ClassCls[_]", subtypes.map(st => q"classOf[${st.tpe}]"))}
      ) {
        def caseDependencies = {
          ..${cachedImplicitDeclarations(ci => q"val ${ci.name} = ${ci.body}")}
          ${mkArray(tq"$GenCodecCls[_]", subtypes.map(_.instance))}
        }
      }
     """
  }

  sealed abstract class CaseInfo {
    def idx: Int
    def subtype: Type
    def applyParams: List[Symbol]
    def depInstance: Tree

    val sym: Symbol = subtype.typeSymbol
    val caseName: String = targetName(sym)
    val classOf: Tree = q"classOf[$subtype]"
    val generated: List[(Symbol, Type)] = generatedMembers(subtype)
    val (defaultCase, transientCase) =
      findAnnotation(sym, DefaultCaseAnnotType).map(_.tree).map {
        case Apply(_, Nil) => (true, false)
        case Apply(_, BooleanLiteral(transient) :: _) => (true, transient)
        case Apply(_, arg :: _) => c.abort(arg.pos, s"Boolean literal expected as @defaultCase `transient` argument")
      }.getOrElse((false, false))

    val targetNames: Map[Symbol, String] = targetNameMap(applyParams ++ generated.map(_._1))
    val membersByName: Map[String, (Symbol, Type)] =
      (applyParams.map(ap => (ap, actualParamType(ap))) ++ generated).map({ case (s, t) => (targetNames(s), (s, t)) }).toMap
    val oooSymbols: List[Symbol] = membersByName.values.iterator.map(_._1).filter(isOutOfOrder).toList
    val oooFieldNames: Set[String] = oooSymbols.iterator.map(targetNames).toSet

    if (isTransparent(sym)) {
      abort(s"You can't use @transparent on case classes of a sealed trait/class with @flatten annotation")
    }
  }

  case class CaseClassInfo(idx: Int, subtype: Type, applyParams: List[Symbol]) extends CaseInfo {
    def depInstance: Tree = c.inferImplicitValue(getType(tq"$GenCodecObj.OOOFieldsObjectCodec[$subtype]")) match {
      case EmptyTree => q"$GenCodecObj.applyUnapplyCodec[$subtype]"
      case tree => tree
    }
  }

  case class CaseObjectInfo(idx: Int, subtype: Type, singleton: Tree) extends CaseInfo {
    def applyParams: List[Symbol] = Nil
    def depInstance: Tree = forSingleton(subtype, singleton)
  }

  def flatForSealedHierarchy(tpe: Type, caseFieldName: String): Tree = {
    val tcTpe = typeClassInstance(tpe)
    val subtypes = knownSubtypes(tpe).getOrElse(abort(s"$tpe is not a sealed hierarchy root"))
    targetNameMap(subtypes.map(_.typeSymbol)) // just to validate uniqueness

    val caseInfos: List[CaseInfo] = subtypes.zipWithIndex.map { case (st, idx) =>
      applyUnapplyFor(st).map { au =>
        CaseClassInfo(idx, st, au.params)
      } orElse singleValueFor(st).map { singleton =>
        CaseObjectInfo(idx, st, singleton)
      } getOrElse abort(
        s"Cannot materialize flat codec for $tpe because $st is not a case class or case object"
      )
    }

    caseInfos.foreach { ci =>
      if (ci.targetNames.values.exists(_ == caseFieldName)) {
        abort(s"$caseFieldName is the case discriminator field name that can't be used as any other's field name")
      }
    }

    val defaultCase: Option[CaseInfo] = caseInfos.filter(_.defaultCase) match {
      case Nil => None
      case List(single) => Some(single)
      case _ => abort(s"Only one class or object may be marked as @defaultCase")
    }

    val oooParams: Map[String, (Symbol, Type)] = caseInfos.flatMap { ci =>
      ci.oooFieldNames.iterator.map(name => (name, ci.membersByName(name)))
    }.toMap
    val oooParamNames = oooParams.keys.toList

    val caseDependentFieldNames =
      (for {
        ci <- caseInfos.iterator
        ap <- ci.applyParams.iterator
        name = ci.targetNames(ap)
        if !oooParams.contains(name)
      } yield name).toSet

    // Make sure that out of order params are marked with @outOfOrder in every case class in which they appear
    // and that they all have exactly the same type.
    for {
      (name, (_, ptpe)) <- oooParams
      ci <- caseInfos
      (otherSym, otherTpe) <- ci.membersByName.get(name)
    } {
      if (!isOutOfOrder(otherSym)) {
        abort(s"Out of order parameter $name must be marked as @outOfOrder in every case class " +
          "(or the annotation may be inherited)")
      }
      if (!(otherTpe =:= ptpe)) {
        abort(s"Out of order parameter $name must have the same type in every case class")
      }
    }

    val oooDependencies = oooParamNames.map { name =>
      val (param, ptpe) = oooParams(name)
      dependency(ptpe, tcTpe, param)
    }

    q"""
      new $SerializationPkg.FlatSealedHierarchyCodec[$tpe](
        ${tpe.toString},
        ${typeOf[Null] <:< tpe},
        ${mkArray(StringCls, caseInfos.map(_.caseName))},
        ${mkArray(tq"$ClassCls[_]", caseInfos.map(_.classOf))},
        ${mkArray(StringCls, oooParamNames)},
        $SetObj(..$caseDependentFieldNames),
        $caseFieldName,
        ${defaultCase.map(caseInfos.indexOf).getOrElse(-1)},
        ${defaultCase.exists(_.transientCase)}
      ) {
        ..$cachedImplicitDeclarations
        def caseDependencies = ${mkArray(tq"$GenCodecObj.OOOFieldsObjectCodec[_]", caseInfos.map(_.depInstance))}
        def oooDependencies = ${mkArray(tq"$GenCodecCls[_]", oooDependencies)}
      }
    """
  }

  def forUnknown(tpe: Type): Tree =
    abort(s"Cannot automatically derive GenCodec for $tpe")

  def materializeRecursively[T: WeakTypeTag]: Tree = instrument {
    val tpe = weakTypeOf[T].dealias
    q"""
       implicit def ${c.freshName(TermName("allow"))}[T]: $AllowImplicitMacroCls[$GenCodecCls[T]] =
         $AllowImplicitMacroObj[$GenCodecCls[T]]
       $GenCodecObj.materialize[$tpe]
     """
  }

  def applyUnapplyCodec[T: WeakTypeTag]: Tree = instrument {
    val tpe = weakTypeOf[T].dealias
    val subTcTpe = typeClassInstance(tpe)
    val au = applyUnapplyFor(tpe).getOrElse(abort(s"$tpe is not a case class or case class like type"))
    val unguarded = forApplyUnapply(au, applyParams(au, subTcTpe))
    withRecursiveImplicitGuard(tpe, unguarded)
  }

  def fromApplyUnapplyProvider[T: WeakTypeTag](applyUnapplyProvider: Tree): Tree = instrument {
    val tpe = weakTypeOf[T].dealias
    val tcTpe = typeClassInstance(tpe)
    applyUnapplyFor(tpe, applyUnapplyProvider) match {
      case Some(au) =>
        forApplyUnapply(au, applyParams(au, tcTpe))
      case None =>
        abort(s"Cannot derive GenCodec for $tpe from `apply` and `unapply`/`unapplySeq` methods of ${applyUnapplyProvider.tpe}")
    }
  }

  def forSealedEnum[T: WeakTypeTag]: Tree = instrument {
    val tpe = weakTypeOf[T].dealias
    q"$GenCodecObj.fromKeyCodec($SerializationPkg.GenKeyCodec.forSealedEnum[$tpe])"
  }

  private def isJavaGetter(ms: MethodSymbol): Boolean =
    ms.typeParams.isEmpty && ms.paramLists == List(Nil) && {
      val expectedPrefix = if (ms.typeSignature.finalResultType =:= typeOf[Boolean]) "is" else "get"
      val nameStr = ms.name.decodedName.toString
      nameStr.startsWith(expectedPrefix) &&
        nameStr.length > expectedPrefix.length &&
        nameStr.charAt(expectedPrefix.length).isUpper
    }

  private def isJavaSetter(ms: MethodSymbol): Boolean =
    ms.typeParams.isEmpty && ms.paramLists.map(_.length) == List(1) && {
      val nameStr = ms.name.decodedName.toString
      nameStr.startsWith("set") && nameStr.length > 3 && nameStr.charAt(3).isUpper
    }

  def fromJavaBuilder[T: WeakTypeTag, B: WeakTypeTag](newBuilder: Tree)(build: Tree): Tree = {
    val ttpe = weakTypeOf[T].dealias
    val btpe = weakTypeOf[B].dealias

    val fieldNames = new mutable.ListBuffer[String]
    val getters = new mutable.ListBuffer[Tree]
    val setters = new mutable.ListBuffer[Tree]
    val deps = new mutable.ListBuffer[Tree]

    ttpe.members.iterator.foreach { getter =>
      if(getter.isMethod && isJavaGetter(getter.asMethod)) {
        val propType = getter.typeSignatureIn(ttpe).finalResultType
        val getterName = getter.name.decodedName.toString
        val setterName = getterName.replaceFirst("^(get|is)", "set")

        val setterOpt = btpe.member(TermName(setterName)).alternatives.find { s =>
          s.isMethod && isJavaSetter(s.asMethod) &&
            s.typeSignatureIn(btpe).paramLists.head.head.typeSignature =:= propType
        }
        setterOpt.foreach { setter =>
          val propName = setterName.charAt(3).toLower.toString + setterName.drop(4)
          val errorCtx = ErrorCtx(s"Cannot materialize GenCodec for $ttpe because of problem with property $propName:\n", getter.pos)
          fieldNames += propName
          deps += inferCachedImplicit(getType(tq"$GenCodecCls[$propType]"), errorCtx).reference(Nil)
          getters += q"(v: $ttpe) => v.$getter()"
          setters += q"(b: $btpe, v: $ScalaPkg.Any) => b.$setter(v.asInstanceOf[$propType])"
        }
      }
    }

    q"""
       new $SerializationPkg.JavaBuilderBasedCodec[$ttpe, $btpe](
         ${ttpe.toString},
         ${typeOf[Null] <:< ttpe},
         $newBuilder,
         $build,
         ${mkArray(StringCls, fieldNames.result())},
         ${mkArray(tq"$ttpe => $ScalaPkg.Any", getters.result())},
         ${mkArray(tq"($btpe, $ScalaPkg.Any) => $btpe", setters.result())},
       ) {
         def dependencies = {
           ..${cachedImplicitDeclarations(ci => q"val ${ci.name} = ${ci.body}")}
           ${mkArray(tq"$GenCodecCls[_]", deps.result())}
         }
       }
       """

  }
}
