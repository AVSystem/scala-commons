package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.TypeClassDerivation

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 10/12/15.
  */
class GenCodecMacros(val c: blackbox.Context) extends TypeClassDerivation {

  import c.universe._

  val SerializationPkg = q"$CommonsPackage.serialization"
  val NameAnnotType = getType(tq"$SerializationPkg.name")
  val TransparentAnnotType = getType(tq"$SerializationPkg.transparent")
  val TransientDefaultAnnotType = getType(tq"$SerializationPkg.transientDefault")
  val JavaInteropObj = q"$CommonsPackage.jiop.JavaInterop"
  val JListObj = q"$JavaInteropObj.JList"
  val JListCls = tq"$JavaInteropObj.JList"
  val ListBufferCls = tq"$CollectionPkg.mutable.ListBuffer"
  val BMapCls = tq"$CollectionPkg.Map"
  val GenCodecObj = q"$SerializationPkg.GenCodec"
  val GenCodecCls = tq"$SerializationPkg.GenCodec"
  val NOptObj = q"$CommonsPackage.misc.NOpt"
  val NOptCls = tq"$CommonsPackage.misc.NOpt"

  def tupleGet(i: Int) = TermName(s"_${i + 1}")

  def annotName(sym: Symbol): String =
    getAnnotations(sym, NameAnnotType).headOption.map(_.tree.children.tail).map {
      case Literal(Constant(str: String)) :: _ => str
      case param :: _ => c.abort(param.pos, s"@name argument must be a string literal")
    }.getOrElse(sym.name.decodedName.toString)

  def isTransparent(sym: Symbol): Boolean =
    getAnnotations(sym, TransparentAnnotType).nonEmpty

  def getAnnotations(sym: Symbol, annotTpe: Type): List[Annotation] = {
    val syms =
      if (sym.isClass) sym.asClass.baseClasses
      else sym :: sym.overrides
    syms.flatMap(_.annotations).filter(_.tree.tpe <:< NameAnnotType)
  }

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

  def typeClass(tpe: Type): Type = getType(tq"$GenCodecCls[$tpe]")
  def implementDeferredInstance(tpe: Type): Tree = q"new $GenCodecObj.Deferred[$tpe]"

  def forSingleton(tpe: Type, singleValueTree: Tree): Tree =
    q"new $GenCodecObj.SingletonCodec[$tpe]($singleValueTree)"

  private def isTransientDefault(param: ApplyParam) =
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
           new $GenCodecObj.NullSafeCodec[$tpe] {
             ${depDeclaration(p)}
             protected def nullable = ${typeOf[Null] <:< tpe}
             protected def read(input: $SerializationPkg.Input): $tpe =
               $companion.apply[..${tpe.typeArgs}](${depNames(p.sym)}.read(input))
             protected def write(output: $SerializationPkg.Output, value: $tpe): Unit =
               $companion.unapply[..${tpe.typeArgs}](value).map(${depNames(p.sym)}.write(output, _)).getOrElse(unapplyFailed)
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
        new $GenCodecObj.RichObjectCodec[$tpe] {
          ..${params.map(depDeclaration)}
          protected def typeRepr = ${tpe.toString}
          protected def nullable = ${typeOf[Null] <:< tpe}
          protected def readObject(input: $SerializationPkg.ObjectInput): $tpe = {
            ..${params.map(p => q"var ${optName(p)}: $NOptCls[${p.sym.typeSignature}] = $NOptObj.Empty")}
            while(input.hasNext) {
              input.nextField() match {
                case ..${params.map(p => cq"(${nameBySym(p.sym)}, fi) => ${optName(p)} = $NOptObj.some(${depNames(p.sym)}.read(fi))")}
                case (_, fi) => fi.skip()
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
      new $GenCodecObj.RichObjectCodec[$tpe] {
        ..${subtypes.map(depDeclaration)}
        protected def typeRepr = ${tpe.toString}
        protected def nullable = ${typeOf[Null] <:< tpe}
        protected def readObject(input: $SerializationPkg.ObjectInput): $tpe = {
          if(input.hasNext) {
            val result = input.nextField() match {
              case ..${subtypes.map(st => cq"(${dbNameBySym(st.sym)}, fi) => ${depNames(st.sym)}.read(fi)")}
              case (key, _) => unknownCase(key)
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
    c.abort(c.enclosingPosition, s"Cannot automatically derive GenCodec for $tpe")
}