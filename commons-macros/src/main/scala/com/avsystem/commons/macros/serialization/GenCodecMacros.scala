package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.TypeClassDerivation

import scala.reflect.macros.blackbox

class GenCodecMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) with TypeClassDerivation {

  import c.universe._

  def mkTupleCodec[T: c.WeakTypeTag](elementCodecs: c.Tree*): c.Tree = {
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

  def typeClass = GenCodecCls
  def typeClassName = "GenCodec"
  def wrapInAuto(tree: Tree) = q"$GenCodecObj.Auto($tree)"
  def implementDeferredInstance(tpe: Type): Tree = q"new $GenCodecObj.Deferred[$tpe]"

  override def materializeFor(tpe: Type) = {
    val tsym = tpe.typeSymbol
    if (isSealedHierarchyRoot(tsym)) getAnnotations(tsym, FlattenAnnotType).headOption match {
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

    if (generated.isEmpty)
      q"new $SerializationPkg.SingletonCodec[$tpe](${tpe.toString}, $singleValueTree)"
    else {
      val tcTpe = typeClassInstance(tpe)
      val targetNames = targetNameMap(generated.map(_._1))
      val depNames = depNamesMap(generated.map(_._1))

      def depDeclaration(sym: Symbol, depTpe: Type) =
        q"lazy val ${depNames(sym)} = ${dependency(depTpe, tcTpe, s"for generated member ${sym.name}")}"

      def generatedWrite(sym: Symbol) =
        q"writeField(${targetNames(sym)}, output, ${mkParamLessCall(q"value", sym)}, ${depNames(sym)})"

      q"""
        new $SerializationPkg.SingletonCodec[$tpe](${tpe.toString}, $singleValueTree) {
          ..${generated.map({ case (sym, depTpe) => depDeclaration(sym, depTpe) })}
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

  def forApplyUnapply(tpe: Type, apply: Symbol, unapply: Symbol, params: List[ApplyParam]): Tree =
    forApplyUnapply(tpe, Ident(tpe.typeSymbol.companion), apply, unapply, params)

  def forApplyUnapply(tpe: Type, companion: Tree, apply: Symbol, unapply: Symbol, params: List[ApplyParam]): Tree = {
    val tcTpe = typeClassInstance(tpe)
    val generated = generatedMembers(tpe)
    val nameBySym = targetNameMap(params.map(_.sym) ++ generated.map(_._1))

    val genDepNames = generated.map({ case (sym, _) => (sym, newDepName(sym)) }).toMap

    def generatedDepDeclaration(sym: Symbol, depTpe: Type) =
      q"lazy val ${genDepNames(sym)} = ${dependency(depTpe, tcTpe, s"for generated member ${sym.name}")}"

    // don't use apply/unapply when they're synthetic (for case class) to avoid reference to companion object

    val caseClass = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass &&
      companion.symbol == tpe.typeSymbol.companion
    val canUseFields = caseClass && unapply.isSynthetic && params.forall { p =>
      alternatives(tpe.member(p.sym.name)).exists { f =>
        f.isTerm && f.asTerm.isCaseAccessor && f.isPublic && f.typeSignature.finalResultType =:= p.sym.typeSignature
      }
    }

    def applier(args: List[Tree]) =
      if (apply.isConstructor) q"new $tpe(..$args)"
      else q"$companion.apply[..${tpe.typeArgs}](..$args)"

    def writeFields = params match {
      case Nil =>
        if (canUseFields)
          q"()"
        else
          q"""
            if(!$companion.$unapply[..${tpe.typeArgs}](value)) {
              unapplyFailed
            }
           """
      case List(p) =>
        def writeField(value: Tree) = {
          val baseWrite = q"writeField[${p.valueType}](output, ${p.idx}, $value)"
          if (isTransientDefault(p))
            q"if($value != ${p.defaultValue}) { $baseWrite }"
          else
            baseWrite
        }

        if (canUseFields)
          writeField(q"value.${p.sym.name}")
        else
          q"""
            val unapplyRes = $companion.$unapply[..${tpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else ${writeField(q"unapplyRes.get")}
           """
      case _ =>
        def writeField(p: ApplyParam, value: Tree) = {
          val baseWrite = q"writeField[${p.valueType}](output, ${p.idx}, $value)"
          if (isTransientDefault(p))
            q"if($value != ${p.defaultValue}) { $baseWrite }"
          else
            baseWrite
        }

        if (canUseFields)
          q"..${params.map(p => writeField(p, q"value.${p.sym.name}"))}"
        else
          q"""
            val unapplyRes = $companion.$unapply[..${tpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else {
              val t = unapplyRes.get
              ..${params.map(p => writeField(p, q"t.${tupleGet(p.idx)}"))}
            }
           """
    }

    if (isTransparent(tpe.typeSymbol)) params match {
      case List(p) =>
        if (generated.nonEmpty) {
          abort(s"class marked as @transparent cannot have @generated members")
        }

        val unwrapBody = if (canUseFields)
          q"value.${p.sym.name}"
        else
          q"""
            val unapplyRes = $companion.$unapply[..${tpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else unapplyRes.get
           """

        q"""
           new $SerializationPkg.TransparentCodec[$tpe,${p.valueType}](
             ${tpe.toString},
             ${typeOf[Null] <:< tpe},
             ${p.instance}
           ) {
             def wrap(underlying: ${p.valueType}): $tpe = ${applier(List(q"underlying"))}
             def unwrap(value: $tpe): ${p.valueType} = $unwrapBody
           }
         """
      case _ =>
        abort(s"@transparent annotation found on class with ${params.size} parameters, expected exactly one.")

    } else {

      def readField(param: ApplyParam) =
        param.defaultValue match {
          case EmptyTree => q"getField[${param.valueType}](fieldValues, ${param.idx})"
          case tree => q"getField[${param.valueType}](fieldValues, ${param.idx}, $tree)"
        }

      def generatedWrite(sym: Symbol) =
        q"writeField(${nameBySym(sym)}, output, ${mkParamLessCall(q"value", sym)}, ${genDepNames(sym)})"

      q"""
        new $SerializationPkg.ApplyUnapplyCodec[$tpe](
          ${tpe.toString},
          ${typeOf[Null] <:< tpe},
          $ScalaPkg.Array[$StringCls](..${params.map(p => nameBySym(p.sym))})
        ) {
          lazy val dependencies = $ScalaPkg.Array[$GenCodecCls[_]](..${params.map(_.instance)})
          ..${generated.collect({ case (sym, depTpe) => generatedDepDeclaration(sym, depTpe) })}
          def instantiate(fieldValues: $SerializationPkg.FieldValues) =
            ${applier(params.map(p => p.asArgument(readField(p))))}
          def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe) = {
            $writeFields
            ..${generated.map({ case (sym, _) => generatedWrite(sym) })}
          }
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
        c.abort(c.enclosingPosition, s"$paramsOrSubclasses ${syms.map(_.name).mkString(", ")} have the same @name: $targetName")
    }
  }

  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree = {
    val targetNameBySym = targetNameMap(subtypes.map(_.sym))
    q"""
      new $SerializationPkg.NestedSealedHierarchyCodec[$tpe](
        ${tpe.toString},
        ${typeOf[Null] <:< tpe},
        $ScalaPkg.Array[$StringCls](..${subtypes.map(st => targetNameBySym(st.sym))})
      ) {
        lazy val caseDependencies = $ScalaPkg.Array[$GenCodecCls[_ <: $tpe]](..${subtypes.map(_.instance)})
        def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe) = value match {
          case ..${subtypes.map(st => cq"value: ${st.tpe} => writeCase(output, ${st.idx}, value)")}
        }
      }
     """
  }

  sealed abstract class CaseInfo {
    def idx: Int
    def subtype: Type
    def applyParams: List[ApplyParam]

    val sym: Symbol = subtype.typeSymbol
    val caseName: String = targetName(sym)
    val generated: List[(Symbol, Type)] = generatedMembers(subtype)
    val (defaultCase, transientCase) =
      getAnnotations(sym, DefaultCaseAnnotType).headOption
        .map(a => a.tree match {
          case Apply(_, Nil) => (true, false)
          case Apply(_, BooleanLiteral(transient) :: _) => (true, transient)
          case Apply(_, arg :: _) => c.abort(arg.pos, s"Boolean literal expected as @defaultCase `transient` argument")
        }).getOrElse((false, false))

    val targetNames: Map[Symbol, String] = targetNameMap(applyParams.map(_.sym) ++ generated.map(_._1))
    val membersByName: Map[String, (Symbol, Type)] =
      (applyParams.map(ap => (ap.sym, ap.valueType)) ++ generated).map({ case (s, t) => (targetNames(s), (s, t)) }).toMap
    val oooSymbols: List[Symbol] = membersByName.values.iterator.map(_._1).filter(isOutOfOrder).toList
    val oooFieldNames: Set[String] = oooSymbols.iterator.map(targetNames).toSet

    def depInstance: Tree
    def caseWrite = cq"value: $subtype => writeCase(output, $idx, $transientCase, value)"

    if (isTransparent(sym)) {
      abort(s"You can't use @transparent on case classes of a sealed trait/class with @flatten annotation")
    }
  }

  case class CaseClassInfo(idx: Int, subtype: Type, applyUnapply: ApplyUnapply, applyParams: List[ApplyParam]) extends CaseInfo {
    def depInstance =
      forApplyUnapply(subtype, applyUnapply.apply, applyUnapply.unapply, applyParams)
  }

  case class CaseObjectInfo(idx: Int, subtype: Type, singleton: Tree) extends CaseInfo {
    def applyParams = Nil
    def depInstance = forSingleton(subtype, singleton)
  }

  def flatForSealedHierarchy(tpe: Type, caseFieldName: String): Tree = {
    val tcTpe = typeClassInstance(tpe)
    val subtypes = knownSubtypes(tpe).getOrElse(abort(s"$tpe is not a sealed hierarchy root"))
    targetNameMap(subtypes.map(_.typeSymbol)) // just to validate uniqueness

    val caseInfos: List[CaseInfo] = subtypes.zipWithIndex.map { case (st, idx) =>
      applyUnapplyFor(st).map { au =>
        val subTcTpe = typeClassInstance(st)
        val applyParams = au.params.zipWithIndex.map { case ((s, defaultValue), pidx) =>
          ApplyParam(pidx, s, defaultValue, dependency(nonRepeatedType(s.typeSignature), subTcTpe, s"for field ${s.name}"))
        }
        CaseClassInfo(idx, st, au, applyParams)
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

    val oooParams: Map[String, Type] = caseInfos.flatMap { ci =>
      ci.oooFieldNames.iterator.map(name => (name, ci.membersByName(name)._2))
    }.toMap
    val oooParamNames = oooParams.keys.toList

    val caseDependentFieldNames =
      (for {
        ci <- caseInfos.iterator
        ap <- ci.applyParams.iterator
        name = ci.targetNames(ap.sym)
        if !oooParams.contains(name)
      } yield name).toSet

    // Make sure that out of order params are marked with @outOfOrder in every case class in which they appear
    // and that they all have exactly the same type.
    for {
      (name, ptpe) <- oooParams
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

    def oooDependency(name: String) =
      dependency(oooParams(name), tcTpe, s"for field $name")

    q"""
      new $SerializationPkg.FlatSealedHierarchyCodec[$tpe](
        ${tpe.toString},
        ${typeOf[Null] <:< tpe},
        $ScalaPkg.Array[$StringCls](..${caseInfos.map(_.caseName)}),
        $ScalaPkg.Array[$StringCls](..$oooParamNames),
        $SetObj(..$caseDependentFieldNames),
        $caseFieldName,
        ${defaultCase.map(caseInfos.indexOf).getOrElse(-1)}
      ) {
        lazy val caseDependencies = $ScalaPkg.Array[$GenCodecObj.OOOFieldsObjectCodec[_ <: $tpe]](..${caseInfos.map(_.depInstance)})
        lazy val oooDependencies = $ScalaPkg.Array[$GenCodecCls[_]](..${oooParamNames.map(oooDependency)})

        def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe) = value match {
          case ..${caseInfos.map(_.caseWrite)}
        }
      }
    """
  }

  def forUnknown(tpe: Type): Tree =
    typecheckException(s"Cannot automatically derive GenCodec for $tpe")

  def materializeRecursively[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    q"""
       implicit def ${c.freshName(TermName("allow"))}[T]: $AllowImplicitMacroCls[$typeClass[T]] =
         $AllowImplicitMacroObj[$typeClass[T]]
       $GenCodecObj.materialize[$tpe]
     """
  }

  def materializeMacroCodec[T: c.WeakTypeTag]: Tree =
    q"$SerializationPkg.MacroCodec($GenCodecObj.materialize[${weakTypeOf[T]}])"

  def fromApplyUnapplyProvider[T: c.WeakTypeTag](applyUnapplyProvider: Tree): Tree = {
    val tpe = weakTypeOf[T]
    val tcTpe = typeClassInstance(tpe)
    applyUnapplyFor(tpe, applyUnapplyProvider) match {
      case Some(ApplyUnapply(apply, unapply, params)) =>
        val dependencies = params.zipWithIndex.map { case ((s, defaultValue), idx) =>
          ApplyParam(idx, s, defaultValue, dependency(nonRepeatedType(s.typeSignature), tcTpe, s"for field ${s.name}"))
        }
        forApplyUnapply(tpe, applyUnapplyProvider, apply, unapply, dependencies)
      case None =>
        abort(s"Cannot derive GenCodec for $tpe from `apply` and `unapply`/`unapplySeq` methods of ${applyUnapplyProvider.tpe}")
    }
  }

  def forSealedEnum[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    q"$GenCodecObj.fromKeyCodec($SerializationPkg.GenKeyCodec.forSealedEnum[$tpe])"
  }
}
