package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.{MacroCommons, TypeClassDerivation}

import scala.reflect.macros.blackbox

trait CodecMacroCommons extends MacroCommons {

  import c.universe._

  val SerializationPkg = q"$CommonsPackage.serialization"
  val NameAnnotType = getType(tq"$SerializationPkg.name")
  val JavaInteropObj = q"$CommonsPackage.jiop.JavaInterop"
  val JListObj = q"$JavaInteropObj.JList"
  val JListCls = tq"$JavaInteropObj.JList"
  val ListBufferCls = tq"$CollectionPkg.mutable.ListBuffer"
  val BMapCls = tq"$CollectionPkg.Map"
  val NOptObj = q"$CommonsPackage.misc.NOpt"
  val NOptCls = tq"$CommonsPackage.misc.NOpt"

  def tupleGet(i: Int) = TermName(s"_${i + 1}")

  def annotName(sym: Symbol): String =
    getAnnotations(sym, NameAnnotType).headOption.map(_.tree.children.tail).map {
      case Literal(Constant(str: String)) :: _ => str
      case param :: _ => c.abort(param.pos, s"@name argument must be a string literal")
    }.getOrElse(sym.name.decodedName.toString)

  def getAnnotations(sym: Symbol, annotTpe: Type): List[Annotation] = {
    val syms =
      if (sym.isClass) sym.asClass.baseClasses
      else sym :: sym.overrides
    syms.flatMap(_.annotations).filter(_.tree.tpe <:< annotTpe)
  }
}

/**
  * Author: ghik
  * Created: 10/12/15.
  */
class GenCodecMacros(val c: blackbox.Context) extends TypeClassDerivation with CodecMacroCommons {

  import c.universe._

  val TransparentAnnotType = getType(tq"$SerializationPkg.transparent")
  val TransientDefaultAnnotType = getType(tq"$SerializationPkg.transientDefault")
  val GenCodecObj = q"$SerializationPkg.GenCodec"
  val GenCodecCls = tq"$SerializationPkg.GenCodec"

  def mkTupleCodec[T: c.WeakTypeTag](elementCodecs: c.Tree*): c.Tree = {
    val tupleTpe = weakTypeOf[T]
    val indices = elementCodecs.indices
    q"""
        new $GenCodecObj.ListCodec[$tupleTpe] {
          protected def nullable = true
          protected def readList(input: $SerializationPkg.ListInput) =
            (..${indices.map(i => q"${elementCodecs(i)}.read(input.nextElement())")})
          protected def writeList(output: $SerializationPkg.ListOutput, value: $tupleTpe) = {
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

  def isTransparent(sym: Symbol): Boolean =
    getAnnotations(sym, TransparentAnnotType).nonEmpty

  def isTransientDefault(param: ApplyParam) =
    param.defaultValue.nonEmpty && param.sym.annotations.exists(_.tree.tpe <:< TransientDefaultAnnotType)

  def forApplyUnapply(tpe: Type, companion: Symbol, params: List[ApplyParam]): Tree = {
    val nameBySym = params.groupBy(p => annotName(p.sym)).map {
      case (name, List(param)) => (param.sym, name)
      case (name, ps) if ps.length > 1 =>
        c.abort(c.enclosingPosition, s"Parameters ${ps.map(_.sym.name).mkString(", ")} have the same @name: $name")
    }
    val depNames = params.map(p => (p.sym, c.freshName(TermName(p.sym.name.toString + "Codec")))).toMap
    def depDeclaration(param: ApplyParam) =
      q"val ${depNames(param.sym)} = ${param.instance}"

    def writeObjectBody = params match {
      case Nil =>
        q"""
          if(!$companion.unapply[..${tpe.typeArgs}](value)) {
            unapplyFailed
          }
         """
      case List(p) =>
        val writeField = {
          val baseWrite = q"${depNames(p.sym)}.write(output.writeField(${nameBySym(p.sym)}), v)"
          if (isTransientDefault(p))
            q"if(v != ${p.defaultValue}) { $baseWrite }"
          else
            baseWrite
        }

        q"""
          $companion.unapply[..${tpe.typeArgs}](value)
            .map(v => $writeField).getOrElse(unapplyFailed)
         """
      case _ =>
        def writeField(p: ApplyParam, idx: Int) = {
          val baseWrite = q"${depNames(p.sym)}.write(output.writeField(${nameBySym(p.sym)}), t.${tupleGet(idx)})"
          if (isTransientDefault(p))
            q"if(t.${tupleGet(idx)} != ${p.defaultValue}) { $baseWrite }"
          else
            baseWrite
        }

        q"""
          $companion.unapply[..${tpe.typeArgs}](value).map { t =>
            ..${params.zipWithIndex.map({ case (p, i) => writeField(p, i) })}
          }.getOrElse(unapplyFailed)
         """
    }

    if (isTransparent(tpe.typeSymbol)) params match {
      case List(p) =>
        q"""
           new $GenCodecObj.NullSafeCodec[$tpe] with $GenCodecObj.ErrorReportingCodec[$tpe] {
             ${depDeclaration(p)}
             protected def typeRepr = ${tpe.toString}
             protected def nullable = ${typeOf[Null] <:< tpe}
             protected def readNonNull(input: $SerializationPkg.Input): $tpe =
               $companion.apply[..${tpe.typeArgs}](${depNames(p.sym)}.read(input))
             protected def writeNonNull(output: $SerializationPkg.Output, value: $tpe): Unit =
               $companion.unapply[..${tpe.typeArgs}](value).map(${depNames(p.sym)}.write(output, _))
               .getOrElse(unapplyFailed)
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
          protected def nullable = ${typeOf[Null] <:< tpe}
          protected def readObject(input: $SerializationPkg.ObjectInput): $tpe = {
            ..${params.map(p => q"var ${optName(p)}: $NOptCls[${p.sym.typeSignature}] = $NOptObj.Empty")}
            while(input.hasNext) {
              val fi = input.nextField()
              fi.fieldName match {
                case ..${params.map(p => cq"${nameBySym(p.sym)} => ${optName(p)} = $NOptObj.some(${depNames(p.sym)}.read(fi))")}
                case _ => fi.skip()
              }
            }
            $companion.apply[..${tpe.typeArgs}](..${params.map(readField)})
          }
          protected def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe) = $writeObjectBody
        }
       """
    }
  }

  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree = {
    val dbNameBySym = subtypes.groupBy(st => annotName(st.sym)).map {
      case (dbName, List(subtype)) => (subtype.sym, dbName)
      case (dbName, kst) =>
        c.abort(c.enclosingPosition, s"Subclasses ${kst.map(_.sym.name).mkString(", ")} have the same @name: $dbName")
    }

    val depNames = subtypes.map(st => (st.sym, c.freshName(TermName(st.sym.name.toString + "Codec")))).toMap
    def depDeclaration(subtype: KnownSubtype) =
      q"val ${depNames(subtype.sym)} = ${subtype.instance}"

    q"""
      new $GenCodecObj.ObjectCodec[$tpe] with $GenCodecObj.ErrorReportingCodec[$tpe] {
        ..${subtypes.map(depDeclaration)}
        protected def typeRepr = ${tpe.toString}
        protected def nullable = ${typeOf[Null] <:< tpe}
        protected def readObject(input: $SerializationPkg.ObjectInput): $tpe = {
          if(input.hasNext) {
            val fi = input.nextField()
            val result = fi.fieldName match {
              case ..${subtypes.map(st => cq"${dbNameBySym(st.sym)} => ${depNames(st.sym)}.read(fi)")}
              case key => unknownCase(key)
            }
            if(input.hasNext) notSingleField(empty = false) else result
          } else notSingleField(empty = true)
        }
        protected def writeObject(output: $SerializationPkg.ObjectOutput, value: $tpe) = value match {
          case ..${subtypes.map(st => cq"value: ${st.tpe} => ${depNames(st.sym)}.write(output.writeField(${dbNameBySym(st.sym)}), value)")}
        }
      }
     """
  }

  def forUnknown(tpe: Type): Tree =
    typecheckException(s"Cannot automatically derive GenCodec for $tpe")

  def materializeRecursively[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    q"""
       implicit val ${c.freshName(TermName("allow"))}: $materializeRecursivelyCls[$typeClass] =
         $materializeRecursivelyObj[$typeClass]
       $GenCodecObj.materialize[$tpe]
     """
  }

  def materializeRecursivelyImplicitly[T: c.WeakTypeTag](allow: Tree): Tree =
    materialize[T]
}


class GenKeyCodecMacros(val c: blackbox.Context) extends CodecMacroCommons {

  import c.universe._

  val GenKeyCodecObj = q"$SerializationPkg.GenKeyCodec"
  val GenKeyCodecCls = tq"$SerializationPkg.GenKeyCodec"

  def forSealedEnum[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    knownSubtypes(tpe).map { subtypes =>
      def singleValue(st: Type) = singleValueFor(st).getOrElse(abort(s"$st is not an object"))
      val nameBySym = subtypes.groupBy(st => annotName(st.typeSymbol)).map {
        case (name, List(subtype)) => (subtype.typeSymbol, name)
        case (name, kst) =>
          abort(s"Objects ${kst.map(_.typeSymbol.name).mkString(", ")} have the same @name: $name")
      }
      val result =
        q"""
          new $GenKeyCodecCls[$tpe] {
            def tpeString = ${tpe.toString}
            def read(key: String): $tpe = key match {
              case ..${subtypes.map(st => cq"${nameBySym(st.typeSymbol)} => ${singleValue(st)}")}
              case _ => throw new $SerializationPkg.GenCodec.ReadFailure(s"Cannot read $$tpeString, unknown object: $$key")
            }
            def write(value: $tpe): String = value match {
              case ..${subtypes.map(st => cq"_: $st => ${nameBySym(st.typeSymbol)}")}
            }
          }
         """
      withKnownSubclassesCheck(result, tpe)
    }.getOrElse(abort(s"$tpe is not a sealed trait or class"))
  }
}
