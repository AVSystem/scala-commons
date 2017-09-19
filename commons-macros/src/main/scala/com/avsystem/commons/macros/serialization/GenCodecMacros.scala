package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.{AbstractMacroCommons, TypeClassDerivation}

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

  def forSingleton(tpe: Type, singleValueTree: Tree): Tree =
    q"new $GenCodecObj.SingletonCodec[$tpe]($singleValueTree)"

  def isTransientDefault(param: ApplyParam): Boolean =
    param.defaultValue.nonEmpty && param.sym.annotations.exists(_.tree.tpe <:< TransientDefaultAnnotType)

  def forApplyUnapply(tpe: Type, apply: Symbol, unapply: Symbol, params: List[ApplyParam]): Tree =
    forApplyUnapply(tpe, Ident(tpe.typeSymbol.companion), apply, unapply, params)

  def forApplyUnapply(tpe: Type, companion: Tree, apply: Symbol, unapply: Symbol, params: List[ApplyParam]): Tree = {
    val nameBySym = params.groupBy(p => annotName(p.sym)).map {
      case (name, List(param)) => (param.sym, name)
      case (name, ps) if ps.length > 1 =>
        c.abort(c.enclosingPosition, s"Parameters ${ps.map(_.sym.name).mkString(", ")} have the same @name: $name")
    }
    val depNames = params.map(p => (p.sym, c.freshName(TermName(p.sym.name.toString + "Codec")))).toMap
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

      q"""
        new $GenCodecObj.ObjectCodec[$tpe] with $GenCodecObj.ErrorReportingCodec[$tpe] {
          ..${params.map(depDeclaration)}
          protected def typeRepr = ${tpe.toString}
          def nullable = ${typeOf[Null] <:< tpe}
          def readObject(input: $SerializationPkg.ObjectInput): $tpe = {
            ..${params.map(p => q"var ${optName(p)}: $NOptCls[${p.valueType}] = $NOptObj.Empty")}
            while(input.hasNext) {
              val fi = input.nextField()
              fi.fieldName match {
                case ..${params.map(p => cq"${nameBySym(p.sym)} => ${optName(p)} = $NOptObj.some(readField(fi, ${depNames(p.sym)}))")}
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

  private def dbNameBySymMap(subtypeSymbols: Seq[Symbol]): Map[Symbol, String] =
    subtypeSymbols.groupBy(st => annotName(st)).map {
      case (dbName, List(subtype)) => (subtype, dbName)
      case (dbName, kst) =>
        c.abort(c.enclosingPosition, s"Subclasses ${kst.map(_.name).mkString(", ")} have the same @name: $dbName")
    }

  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree = {
    val dbNameBySym = dbNameBySymMap(subtypes.map(_.sym))
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
              case ..${subtypes.map(st => cq"${dbNameBySym(st.sym)} => readCase(fi, ${depNames(st.sym)})")}
              case key => unknownCase(key)
            }
            if(input.hasNext) notSingleField(empty = false) else result
          } else notSingleField(empty = true)
        }
        def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe) = value match {
          case ..${subtypes.map(st => cq"value: ${st.tpe} => writeCase(${dbNameBySym(st.sym)}, output, value, ${depNames(st.sym)})")}
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
        forApplyUnapply(tpe, applyUnapplyProvider, apply, unapply, dependencies)
      case None =>
        abort(s"Cannot derive GenCodec for $tpe from `apply` and `unapply/unapplySeq` methods of ${applyUnapplyProvider.tpe}")
    }
  }

  def forSealedEnum[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    q"$GenCodecObj.fromKeyCodec($SerializationPkg.GenKeyCodec.forSealedEnum[$tpe])"
  }
}
