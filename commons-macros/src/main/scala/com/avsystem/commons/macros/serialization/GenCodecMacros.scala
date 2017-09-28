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
    if (isSealedHierarchyRoot(tsym) && hasAnnotation(tsym, FlattenAnnotType))
      flatForSealedHierarchy(tpe)
    else
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
      q"new $GenCodecObj.SingletonCodec[$tpe]($singleValueTree)"
    else {
      val tcTpe = typeClassInstance(tpe)
      val targetNames = targetNameMap(generated.map(_._1))
      val depNames = depNamesMap(generated.map(_._1))

      def depDeclaration(sym: Symbol, depTpe: Type) =
        q"lazy val ${depNames(sym)} = ${dependency(depTpe, tcTpe, s"for generated member ${sym.name}")}"

      def generatedWrite(sym: Symbol) =
        q"writeField(${targetNames(sym)}, output, ${mkParamLessCall(q"value", sym)}, ${depNames(sym)})"

      q"""
        new $GenCodecObj.SingletonCodec[$tpe]($singleValueTree) with $GenCodecObj.ErrorReportingCodec[$tpe] {
          ..${generated.map({ case (sym, depTpe) => depDeclaration(sym, depTpe) })}
          protected def typeRepr = ${tpe.toString}
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
    forApplyUnapply(tpe, apply, unapply, params, Map.empty)

  def forApplyUnapply(tpe: Type, apply: Symbol, unapply: Symbol,
    params: List[ApplyParam], oooFields: Map[String, TermName]): Tree =
    forApplyUnapply(tpe, Ident(tpe.typeSymbol.companion), apply, unapply, params, oooFields)

  def forApplyUnapply(tpe: Type, companion: Tree, apply: Symbol, unapply: Symbol,
    params: List[ApplyParam], oooFields: Map[String, TermName]): Tree = {

    val tcTpe = typeClassInstance(tpe)
    val generated = generatedMembers(tpe)
    val nameBySym = targetNameMap(params.map(_.sym) ++ generated.map(_._1))

    def memberDepName(sym: Symbol) = oooFields.getOrElse(nameBySym(sym), newDepName(sym))

    val depNames = params.map(p => (p.sym, memberDepName(p.sym))).toMap ++
      generated.map({ case (sym, _) => (sym, memberDepName(sym)) }).toMap

    def depDeclaration(param: ApplyParam) =
      q"lazy val ${depNames(param.sym)} = ${param.instance}"

    def generatedDepDeclaration(sym: Symbol, depTpe: Type) =
      q"lazy val ${depNames(sym)} = ${dependency(depTpe, tcTpe, s"for generated member ${sym.name}")}"

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
          val baseWrite = q"writeField(${nameBySym(p.sym)}, output, $value, ${depNames(p.sym)})"
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
          val baseWrite = q"writeField(${nameBySym(p.sym)}, output, $value, ${depNames(p.sym)})"
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
              ..${params.zipWithIndex.map({ case (p, i) => writeField(p, q"t.${tupleGet(i)}") })}
            }
           """
    }

    if (isTransparent(tpe.typeSymbol)) params match {
      case List(p) =>
        if (generated.nonEmpty) {
          abort(s"class marked as @transparent cannot have @generated members")
        }

        val writeBody = if (canUseFields)
          q"${depNames(p.sym)}.write(output, value.${p.sym.name})"
        else
          q"""
            val unapplyRes = $companion.$unapply[..${tpe.typeArgs}](value)
            if(unapplyRes.isEmpty) unapplyFailed else ${depNames(p.sym)}.write(output, unapplyRes.get)
           """

        q"""
           new $GenCodecObj.NullSafeCodec[$tpe] with $GenCodecObj.ErrorReportingCodec[$tpe] {
             ${depDeclaration(p)}
             protected def typeRepr = ${tpe.toString}
             def nullable = ${typeOf[Null] <:< tpe}
             def readNonNull(input: $SerializationPkg.Input): $tpe =
               ${applier(List(q"${depNames(p.sym)}.read(input)"))}
             def writeNonNull(output: $SerializationPkg.Output, value: $tpe): $UnitCls =
               $writeBody
           }
         """
      case _ =>
        abort(s"@transparent annotation found on class with ${params.size} parameters, expected exactly one.")
    } else {

      def optName(param: ApplyParam): TermName =
        TermName(nameBySym(param.sym) + "Opt")

      def readField(param: ApplyParam) = {
        val fieldName = nameBySym(param.sym)
        val defaultValueTree = param.defaultValue.orElse(q"fieldMissing($fieldName)")
        q"${optName(param)}.getOrElse($defaultValueTree)"
      }

      def varDeclaration(p: ApplyParam) = {
        val varName = optName(p)
        if (oooFields.contains(nameBySym(p.sym)))
          q"var $varName = oooFields.$varName"
        else
          q"var $varName = $NOptObj.empty[${p.valueType}]"
      }

      val baseClass =
        if (oooFields.nonEmpty) tq"$GenCodecObj.ApplyUnapplyCodec[$tpe,OutOfOrderFields]"
        else tq"$GenCodecObj.ObjectCodec[$tpe]"

      val oooFieldsParam =
        if (oooFields.nonEmpty) List(q"oooFields: OutOfOrderFields") else Nil

      val emptyOooFieldsDecl =
        if (oooFields.nonEmpty) q"protected val emptyOutOfOrderFields = new OutOfOrderFields" else q"()"

      def fieldReadCase(p: ApplyParam): Tree =
        cq"${nameBySym(p.sym)} => ${optName(p)} = $NOptObj.some(readField(fi, ${depNames(p.sym)}))"

      def generatedWrite(sym: Symbol) =
        q"writeField(${nameBySym(sym)}, output, ${mkParamLessCall(q"value", sym)}, ${depNames(sym)})"

      q"""
        new $baseClass with $GenCodecObj.ErrorReportingCodec[$tpe] {
          ..${params.filterNot(ap => oooFields.contains(nameBySym(ap.sym))).map(depDeclaration)}
          ..${generated.collect({ case (sym, depTpe) if !oooFields.contains(nameBySym(sym)) => generatedDepDeclaration(sym, depTpe) })}
          $emptyOooFieldsDecl
          protected def typeRepr = ${tpe.toString}
          def nullable = ${typeOf[Null] <:< tpe}
          def readObject(input: $SerializationPkg.ObjectInput, ..$oooFieldsParam): $tpe = {
            ..${params.map(varDeclaration)}
            while(input.hasNext) {
              val fi = input.nextField()
              fi.fieldName match {
                case ..${params.map(fieldReadCase)}
                case _ => fi.skip()
              }
            }
            ${applier(params.map(p => p.asArgument(readField(p))))}
          }
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
    val depNames = depNamesMap(subtypes.map(_.sym))

    def depDeclaration(subtype: KnownSubtype) =
      q"lazy val ${depNames(subtype.sym)} = ${subtype.instance}"

    q"""
      new $GenCodecObj.ObjectCodec[$tpe] with $GenCodecObj.ErrorReportingCodec[$tpe] {
        ..${subtypes.map(depDeclaration)}
        protected def typeRepr = ${tpe.toString}
        def nullable = ${typeOf[Null] <:< tpe}
        def readObject(input: $SerializationPkg.ObjectInput): $tpe = {
          if(input.hasNext) {
            val fi = input.nextField()
            val result = fi.fieldName match {
              case ..${subtypes.map(st => cq"${targetNameBySym(st.sym)} => readCase(${targetNameBySym(st.sym)}, fi, ${depNames(st.sym)})")}
              case key => unknownCase(key)
            }
            if(input.hasNext) notSingleField(empty = false) else result
          } else notSingleField(empty = true)
        }
        def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe) = value match {
          case ..${subtypes.map(st => cq"value: ${st.tpe} => writeCase(${targetNameBySym(st.sym)}, output, value, ${depNames(st.sym)})")}
        }
      }
     """
  }

  sealed abstract class CaseInfo {
    def subtype: Type
    def applyParams: List[ApplyParam]

    val sym: Symbol = subtype.typeSymbol
    val caseName: String = targetName(sym)
    val depName: TermName = newDepName(sym)
    val generated: List[(Symbol, Type)] = generatedMembers(subtype)

    val targetNames: Map[Symbol, String] = targetNameMap(applyParams.map(_.sym) ++ generated.map(_._1))
    val membersByName: Map[String, (Symbol, Type)] =
      (applyParams.map(ap => (ap.sym, ap.valueType)) ++ generated).map({ case (s, t) => (targetNames(s), (s, t)) }).toMap

    if (targetNames.values.exists(_ == CaseField)) {
      abort(s"`$CaseField` is a reserved field name")
    }
    val oooSymbols: List[Symbol] = membersByName.values.iterator.map(_._1).filter(isOutOfOrder).toList
    val oooFieldNames: Set[String] = oooSymbols.iterator.map(targetNames).toSet

    def depInstance(oooDepNames: Map[String, TermName]): Tree
    def caseRead: Tree

    def depDeclaration(oooDepNames: Map[String, TermName]) = q"lazy val $depName = ${depInstance(oooDepNames)}"
    def caseWrite = cq"value: $subtype => writeFlatCase($caseName, output, value, $depName)"

    if (isTransparent(sym)) {
      abort(s"You can't use @transparent on case classes of a sealed trait/class with @flatten annotation")
    }
  }

  case class CaseClassInfo(subtype: Type, applyUnapply: ApplyUnapply, applyParams: List[ApplyParam]) extends CaseInfo {
    def depInstance(oooDepNames: Map[String, TermName]) =
      forApplyUnapply(subtype, applyUnapply.apply, applyUnapply.unapply, applyParams, oooDepNames.filterKeys(oooFieldNames))
    def caseRead =
      if (oooFieldNames.isEmpty) cq"$caseName => readFlatCase($caseName, input, $depName)"
      else cq"$caseName => readFlatCase($caseName, oooFields, input, $depName)"
  }

  case class CaseObjectInfo(subtype: Type, singleton: Tree) extends CaseInfo {
    def applyParams = Nil
    def depInstance(oooDepNames: Map[String, TermName]) = forSingleton(subtype, singleton)
    def caseRead = cq"$caseName => readFlatCase($caseName, input, $depName)"
  }

  def flatForSealedHierarchy(tpe: Type): Tree = {
    val tcTpe = typeClassInstance(tpe)
    val subtypes = knownSubtypes(tpe).getOrElse(abort(s"$tpe is not a sealed hierarchy root"))
    targetNameMap(subtypes.map(_.typeSymbol)) // just to validate uniqueness

    val caseInfos: List[CaseInfo] = subtypes.map(st => applyUnapplyFor(st).map { au =>
      val subTcTpe = typeClassInstance(st)
      val applyParams = au.params.map { case (s, defaultValue) =>
        ApplyParam(s, defaultValue, dependency(nonRepeatedType(s.typeSignature), subTcTpe, s"for field ${s.name}"))
      }
      CaseClassInfo(st, au, applyParams)
    } orElse singleValueFor(st).map { singleton =>
      CaseObjectInfo(st, singleton)
    } getOrElse abort(s""))

    val oooParams: Map[String, Type] = caseInfos.flatMap { ci =>
      ci.oooFieldNames.iterator.map(name => (name, ci.membersByName(name)._2))
    }.toMap

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

    def optName(name: String) = TermName(name + "Opt")

    val oooDepNames = oooParams.map({ case (name, _) => (name, newDepName(name)) })

    def oooDepDeclaration(name: String, ptpe: Type) =
      q"lazy val ${oooDepNames(name)} = ${dependency(ptpe, tcTpe, s"for field $name")}"

    def oooFieldReadCase(name: String) =
      cq"$name => oooFields.${optName(name)} = $NOptObj.some(readField(fi, ${oooDepNames(name)})); lookForCase()"

    val oooFieldsClass = if (oooParams.isEmpty) q"()" else
      q"""
        final class OutOfOrderFields {
          ..${oooParams.map({ case (name, ptpe) => q"var ${optName(name)} = $NOptObj.empty[$ptpe]" })}
        }
       """

    val oooFieldsDecl =
      if (oooParams.isEmpty) q"()" else q"val oooFields = new OutOfOrderFields"

    val caseDependentParamsCase =
      if(caseDependentFieldNames.isEmpty) Nil
      else {
        val pattern = caseDependentFieldNames.map(n => pq"$n").reduce((p1, p2) => pq"$p1 | $p2")
        List(cq"$pattern => missingCase(fi.fieldName)")
      }

    q"""
      new $GenCodecObj.ObjectCodec[$tpe] with $GenCodecObj.ErrorReportingCodec[$tpe] {
        $oooFieldsClass
        ..${oooParams.map({ case (name, ptpe) => oooDepDeclaration(name, ptpe) })}
        ..${caseInfos.map(_.depDeclaration(oooDepNames))}
        protected def typeRepr = ${tpe.toString}
        def nullable = ${typeOf[Null] <:< tpe}
        def readObject(input: $SerializationPkg.ObjectInput): $tpe = {
          $oooFieldsDecl
          def lookForCase(): $SerializationPkg.FieldInput =
            if(input.hasNext) {
              val fi = input.nextField()
              fi.fieldName match {
                case $CaseField => fi
                case ..${oooParams.keysIterator.map(oooFieldReadCase).toList}
                case ..$caseDependentParamsCase
                case _ => fi.skip(); lookForCase()
              }
            } else missingCase
          readCaseName(lookForCase()) match {
            case ..${caseInfos.map(_.caseRead)}
            case caseName => unknownCase(caseName)
          }
        }

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
        val dependencies = params.map { case (s, defaultValue) =>
          ApplyParam(s, defaultValue, dependency(nonRepeatedType(s.typeSignature), tcTpe, s"for field ${s.name}"))
        }
        forApplyUnapply(tpe, applyUnapplyProvider, apply, unapply, dependencies, Map.empty)
      case None =>
        abort(s"Cannot derive GenCodec for $tpe from `apply` and `unapply`/`unapplySeq` methods of ${applyUnapplyProvider.tpe}")
    }
  }

  def forSealedEnum[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    q"$GenCodecObj.fromKeyCodec($SerializationPkg.GenKeyCodec.forSealedEnum[$tpe])"
  }
}
