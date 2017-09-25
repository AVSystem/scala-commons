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

  def forSingleton(tpe: Type, singleValueTree: Tree): Tree =
    q"new $GenCodecObj.SingletonCodec[$tpe]($singleValueTree)"

  def isTransientDefault(param: ApplyParam): Boolean =
    param.defaultValue.nonEmpty && hasAnnotation(param.sym, TransientDefaultAnnotType)

  def isOutOfOrder(param: ApplyParam): Boolean =
    hasAnnotation(param.sym, OutOfOrderAnnotType)

  def forApplyUnapply(tpe: Type, apply: Symbol, unapply: Symbol, params: List[ApplyParam]): Tree =
    forApplyUnapply(tpe, apply, unapply, params, Map.empty)

  def forApplyUnapply(tpe: Type, apply: Symbol, unapply: Symbol,
    params: List[ApplyParam], oooFields: Map[String, TermName]): Tree =
    forApplyUnapply(tpe, Ident(tpe.typeSymbol.companion), apply, unapply, params, oooFields)

  def forApplyUnapply(tpe: Type, companion: Tree, apply: Symbol, unapply: Symbol,
    params: List[ApplyParam], oooFields: Map[String, TermName]): Tree = {

    val nameBySym = targetNameMap(params.map(_.sym))
    val depNames = params.map { p =>
      (p.sym, oooFields.getOrElse(nameBySym(p.sym), c.freshName(TermName(p.sym.name.toString + "Codec"))))
    }.toMap

    def depDeclaration(param: ApplyParam) =
      q"lazy val ${depNames(param.sym)} = ${param.instance}"

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

    def writeObjectBody = params match {
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

      q"""
        new $baseClass with $GenCodecObj.ErrorReportingCodec[$tpe] {
          ..${params.filterNot(ap => oooFields.contains(nameBySym(ap.sym))).map(depDeclaration)}
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
          def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe) = $writeObjectBody
        }
       """
    }
  }

  private def targetNameMap(symbols: Seq[Symbol]): Map[Symbol, String] = {
    val paramsOrSubclasses = if (symbols.exists(_.isTerm)) "Parameters" else "Subclasses"
    symbols.groupBy(st => targetName(st)).map {
      case (dbName, List(sym)) => (sym, dbName)
      case (dbName, syms) =>
        c.abort(c.enclosingPosition, s"$paramsOrSubclasses ${syms.map(_.name).mkString(", ")} have the same @name: $dbName")
    }
  }

  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree = {
    val targetNameBySym = targetNameMap(subtypes.map(_.sym))
    val depNames = subtypes.map(st => (st.sym, c.freshName(TermName(st.sym.name.toString + "Codec")))).toMap

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
    val sym: Symbol = subtype.typeSymbol
    val caseName: String = targetName(sym)
    val depName: TermName = c.freshName(TermName(sym.name.toString + "Codec"))

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
      forApplyUnapply(subtype, applyUnapply.apply, applyUnapply.unapply, applyParams, oooDepNames.filterKeys(oooFields))
    def caseRead =
      if (oooFields.isEmpty) cq"$caseName => readFlatCase($caseName, input, $depName)"
      else cq"$caseName => readFlatCase($caseName, oooFields, input, $depName)"

    val targetParamNames: Map[Symbol, String] = targetNameMap(applyParams.map(_.sym))
    val paramByName: Map[String, ApplyParam] = applyParams.iterator.map(ap => (targetParamNames(ap.sym), ap)).toMap
    if (paramByName.contains(CaseField)) {
      abort(s"`$CaseField` is a reserved field name")
    }
    val oooParams: List[ApplyParam] = applyParams.filter(isOutOfOrder)
    val oooFields: Set[String] = oooParams.iterator.map(ap => targetParamNames(ap.sym)).toSet
  }

  case class CaseObjectInfo(subtype: Type, singleton: Tree) extends CaseInfo {
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

    val oooParams = caseInfos.flatMap {
      case cci: CaseClassInfo =>
        cci.oooParams.iterator.map(ap => (cci.targetParamNames(ap.sym), ap.valueType))
      case _ => Nil
    }.toMap

    // Make sure that out of order params are marked with @outOfOrder in every case class in which they appear
    // and that they all have exactly the same type.
    for {
      (name, ptpe) <- oooParams
      cci@CaseClassInfo(_, _, _) <- caseInfos
      otherParam <- cci.paramByName.get(name)
    } {
      if (!isOutOfOrder(otherParam)) {
        abort(s"Out of order parameter $name must be marked as @outOfOrder in every case class " +
          "(or the annotation may be inherited)")
      }
      if (!(otherParam.valueType =:= ptpe)) {
        abort(s"Out of order parameter $name must have the same type in every case class")
      }
    }

    def optName(name: String) = TermName(name + "Opt")

    val oooDepNames = oooParams.map({ case (name, _) => (name, c.freshName(TermName(name + "Codec"))) })

    def oooDepDeclaration(name: String, ptpe: Type) =
      q"lazy val ${oooDepNames(name)} = ${dependency(ptpe, tcTpe, s"for field $name")}"

    def oooFieldReadCase(name: String) =
      cq"$name => oooFields.${optName(name)} = $NOptObj.some(readField(fi, ${oooDepNames(name)}))"

    val oooFieldsClass = if (oooParams.isEmpty) q"()" else
      q"""
        final class OutOfOrderFields {
          ..${oooParams.map({ case (name, ptpe) => q"var ${optName(name)} = $NOptObj.empty[$ptpe]" })}
        }
       """

    val oooFieldsDecl =
      if (oooParams.isEmpty) q"()" else q"val oooFields = new OutOfOrderFields"

    q"""
      new $GenCodecObj.ObjectCodec[$tpe] with $GenCodecObj.ErrorReportingCodec[$tpe] {
        $oooFieldsClass
        ..${oooParams.map({ case (name, ptpe) => oooDepDeclaration(name, ptpe) })}
        ..${caseInfos.map(_.depDeclaration(oooDepNames))}
        protected def typeRepr = ${tpe.toString}
        def nullable = ${typeOf[Null] <:< tpe}
        def readObject(input: $SerializationPkg.ObjectInput): $tpe = {
          var lookForCase = true
          var caseField = $OptObj.empty[$SerializationPkg.FieldInput]
          $oooFieldsDecl
          while(lookForCase && input.hasNext) {
            val fi = input.nextField()
            fi.fieldName match {
              case $CaseField =>
                caseField = fi.opt
                lookForCase = false
              case ..${oooParams.keysIterator.map(oooFieldReadCase).toList}
              case _ =>
                lookForCase = false
            }
          }
          readCaseName(caseField) match {
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
        abort(s"Cannot derive GenCodec for $tpe from `apply` and `unapply/unapplySeq` methods of ${applyUnapplyProvider.tpe}")
    }
  }

  def forSealedEnum[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    q"$GenCodecObj.fromKeyCodec($SerializationPkg.GenKeyCodec.forSealedEnum[$tpe])"
  }
}
