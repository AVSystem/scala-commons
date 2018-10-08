package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.TypeClassDerivation

import scala.reflect.macros.blackbox

class GenCodecMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) with TypeClassDerivation {

  import c.universe._

  def mkTupleCodec[T: WeakTypeTag](elementCodecs: Tree*): Tree = showOnDebug {
    val tupleTpe = weakTypeOf[T]
    val indices = elementCodecs.indices
    q"""
      new $GenCodecObj.ListCodec[$tupleTpe] {
        def nullable = true
        def readList(input: $SerializationPkg.ListInput) =
          (..${indices.map(i => q"${elementCodecs(i)}.read(input.nextElement())")})
        def writeList(output: $SerializationPkg.ListOutput, value: $tupleTpe) = {
          ..${indices.map(i => q"${elementCodecs(i)}.write(output.writeElement(), value.${tupleGet(i)})")}
        }
      }
     """
  }

  def typeClass: Tree = GenCodecCls
  def typeClassName: String = "GenCodec"
  def implementDeferredInstance(tpe: Type): Tree = q"new $GenCodecObj.Deferred[$tpe]"

  override def dependency(depTpe: Type, tcTpe: Type, param: Symbol): Tree = {
    val clue = s"Cannot materialize $tcTpe because of problem with parameter ${param.name}: "
    val depTcTpe = typeClassInstance(depTpe)
    Ident(inferCachedImplicit(depTcTpe, clue, param.pos))
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
    def safeSingleValue: Tree = replaceCompanion(c.typecheck(singleValueTree))

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
          override def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe): $UnitCls = {
            ..${generated.map({ case (sym, _) => generatedWrite(sym) })}
          }
        }
      """
    }
  }

  def isTransientDefault(param: ApplyParam): Boolean =
    param.defaultValue.nonEmpty && hasAnnotation(param.sym, TransientDefaultAnnotType)

  def isOutOfOrder(sym: Symbol): Boolean =
    hasAnnotation(sym, OutOfOrderAnnotType)

  def forApplyUnapply(applyUnapply: ApplyUnapply, applyParams: List[ApplyParam]): Tree = {
    val ApplyUnapply(tpe, companion, apply, unapply, _) = applyUnapply
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

    def safeCompanion: Tree = replaceCompanion(companion)

    def applier(args: List[Tree]): Tree =
      if (apply.isConstructor) q"new $dtpe(..$args)"
      else q"$safeCompanion.apply[..${dtpe.typeArgs}](..$args)"

    def writeFields = params match {
      case Nil =>
        if (canUseFields)
          q"()"
        else
          q"""
            if(!$safeCompanion.$unapply[..${dtpe.typeArgs}](value)) {
              unapplyFailed
            }
           """
      case List(p) =>
        def writeField(value: Tree) = {
          val writeArgs = q"output" :: q"${p.idx}" :: value ::
            (if (isTransientDefault(p)) List(p.defaultValue) else Nil)
          q"writeField[${p.valueType}](..$writeArgs)"
        }

        if (canUseFields)
          writeField(q"value.${p.sym.name}")
        else
          q"""
            val unapplyRes = $safeCompanion.$unapply[..${dtpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else ${writeField(q"unapplyRes.get")}
           """
      case _ =>
        def writeField(p: ApplyParam, value: Tree) = {
          val writeArgs = q"output" :: q"${p.idx}" :: value ::
            (if (isTransientDefault(p)) List(p.defaultValue) else Nil)
          q"writeField[${p.valueType}](..$writeArgs)"
        }

        if (canUseFields)
          q"..${params.map(p => writeField(p, q"value.${p.sym.name}"))}"
        else
          q"""
            val unapplyRes = $safeCompanion.$unapply[..${dtpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else {
              val t = unapplyRes.get
              ..${params.map(p => writeField(p, q"t.${tupleGet(p.idx)}"))}
            }
           """
    }

    if (isTransparent(dtpe.typeSymbol)) params match {
      case List(p) =>
        if (generated.nonEmpty) {
          abort(s"class marked as @transparent cannot have @generated members")
        }

        val unwrapBody = if (canUseFields)
          q"value.${p.sym.name}"
        else
          q"""
            val unapplyRes = $safeCompanion.$unapply[..${dtpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else unapplyRes.get
           """

        q"""
          new $SerializationPkg.TransparentCodec[$dtpe,${p.valueType}](
            ${dtpe.toString}
          ) {
            ..$cachedImplicitDeclarations
            def underlyingCodec: $GenCodecCls[${p.valueType}] = ${p.instance}
            def wrap(underlying: ${p.valueType}): $dtpe = ${applier(List(q"underlying"))}
            def unwrap(value: $dtpe): ${p.valueType} = $unwrapBody
          }
         """
      case _ =>
        abort(s"@transparent annotation found on class with ${params.size} parameters, expected exactly one.")

    } else {

      def readField(param: ApplyParam): Tree =
        param.defaultValue match {
          case EmptyTree => q"getField[${param.valueType}](fieldValues, ${param.idx})"
          case tree => q"getField[${param.valueType}](fieldValues, ${param.idx}, $tree)"
        }

      def generatedWrite(sym: Symbol) =
        q"writeField(${nameBySym(sym)}, output, ${mkParamLessCall(q"value", sym)}, ${genDepNames(sym)})"

      val useProductCodec = canUseFields && generated.isEmpty && !params.exists(isTransientDefault)
      val baseClass = TypeName(if (useProductCodec) "ProductCodec" else "ApplyUnapplyCodec")

      def writeMethod = if (useProductCodec) q"()" else
        q"""
          def writeObject(output: $SerializationPkg.ObjectOutput, value: $dtpe) = {
            $writeFields
            ..${generated.map({ case (sym, _) => generatedWrite(sym) })}
          }
         """

      q"""
        new $SerializationPkg.$baseClass[$dtpe](
          ${dtpe.toString},
          ${typeOf[Null] <:< dtpe},
          ${mkArray(StringCls, params.map(p => nameBySym(p.sym)))}
        ) {
          def dependencies = {
            ..${cachedImplicitDeclarations((n, b) => q"val $n = $b")}
            ${mkArray(tq"$GenCodecCls[_]", params.map(_.instance))}
          }
          ..${generated.collect({ case (sym, depTpe) => generatedDepDeclaration(sym, depTpe) })}
          def instantiate(fieldValues: $SerializationPkg.FieldValues) =
            ${applier(params.map(p => p.asArgument(readField(p))))}
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
        ${mkArray(tq"$ClassCls[_ <: $tpe]", subtypes.map(st => q"classOf[${st.tpe}]"))}
      ) {
        def caseDependencies = {
          ..${cachedImplicitDeclarations((n, b) => q"val $n = $b")}
          ${mkArray(tq"$GenCodecCls[_ <: $tpe]", subtypes.map(_.instance))}
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
    def depInstance: Tree =
      q"${c.prefix}.applyUnapplyCodec[$subtype]"
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
      } getOrElse abort(s"")
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
        ${mkArray(tq"$ClassCls[_ <: $tpe]", caseInfos.map(_.classOf))},
        ${mkArray(StringCls, oooParamNames)},
        $SetObj(..$caseDependentFieldNames),
        $caseFieldName,
        ${defaultCase.map(caseInfos.indexOf).getOrElse(-1)},
        ${defaultCase.exists(_.transientCase)}
      ) {
        ..$cachedImplicitDeclarations
        def caseDependencies = ${mkArray(tq"$GenCodecObj.OOOFieldsObjectCodec[_ <: $tpe]", caseInfos.map(_.depInstance))}
        def oooDependencies = ${mkArray(tq"$GenCodecCls[_]", oooDependencies)}
      }
    """
  }

  def forUnknown(tpe: Type): Tree =
    abort(s"Cannot automatically derive GenCodec for $tpe")

  def materializeRecursively[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    q"""
       implicit def ${c.freshName(TermName("allow"))}[T]: $AllowImplicitMacroCls[$typeClass[T]] =
         $AllowImplicitMacroObj[$typeClass[T]]
       $GenCodecObj.materialize[$tpe]
     """
  }

  def applyUnapplyCodec[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    val subTcTpe = typeClassInstance(tpe)
    val au = applyUnapplyFor(tpe).getOrElse(abort(s"$tpe is not a case class or case class like type"))
    val applyParams = au.params.zipWithIndex.map { case (s, pidx) =>
      val defaultValue = au.defaultValueFor(s, pidx)
      ApplyParam(pidx, s, defaultValue, dependency(actualParamType(s), subTcTpe, s))
    }
    val unguarded = forApplyUnapply(au, applyParams)
    withRecursiveImplicitGuard(tpe, unguarded)
  }

  def fromApplyUnapplyProvider[T: WeakTypeTag](applyUnapplyProvider: Tree): Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    val tcTpe = typeClassInstance(tpe)
    applyUnapplyFor(tpe, applyUnapplyProvider) match {
      case Some(au) =>
        val dependencies = au.params.zipWithIndex.map { case (s, pidx) =>
          val defaultValue = au.defaultValueFor(s, pidx)
          ApplyParam(pidx, s, defaultValue, dependency(actualParamType(s), tcTpe, s))
        }
        forApplyUnapply(au, dependencies)
      case None =>
        abort(s"Cannot derive GenCodec for $tpe from `apply` and `unapply`/`unapplySeq` methods of ${applyUnapplyProvider.tpe}")
    }
  }

  def forSealedEnum[T: WeakTypeTag]: Tree = showOnDebug {
    val tpe = weakTypeOf[T].dealias
    q"$GenCodecObj.fromKeyCodec($SerializationPkg.GenKeyCodec.forSealedEnum[$tpe])"
  }
}
