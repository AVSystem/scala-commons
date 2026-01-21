package com.avsystem.commons
package misc

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

/** Typeclass that contains string representation of a concrete type. This representation should correctly parse and
  * typecheck when used as a type in Scala source code.
  *
  * Instances of `TypeString` are implicitly macro-materialized. The macro will fail if the type contains references to
  * local symbols, i.e. symbols that only exist in limited scope and cannot be referred to from any place in source
  * code. This includes type parameters, this-references to enclosing classes, etc.
  *
  * For example, the code below will NOT compile:
  * {{{
  *   def listTypeRepr[T]: String = TypeString.of[List[T]]
  * }}}
  * because `T` is a local symbol that only has meaning inside its own method. However, if you provide external
  * `TypeString` instance for `T`, the macro will pick it up and no longer complain:
  * {{{
  *   def listTypeRepr[T: TypeString]: String = TypeString.of[List[T]]
  * }}}
  * Then, `listTypeRepr[Int]` will produce a string `"List[Int]"`
  */
class TypeString[T](val value: String) extends AnyVal {
  override def toString: String = value
}
object TypeString {
  def apply[T](implicit ts: TypeString[T]): TypeString[T] = ts
  def of[T: TypeString]: String = TypeString[T].value

  implicit def materialize[T]: TypeString[T] = macro macros.misc.MiscMacros.typeString[T]

  implicit val keyCodec: GenKeyCodec[TypeString[?]] =
    GenKeyCodec.create[TypeString[?]](new TypeString(_), _.value)

  implicit val codec: GenCodec[TypeString[?]] =
    GenCodec.nonNullSimple[TypeString[?]](i => new TypeString(i.readString()), (o, ts) => o.writeString(ts.value))
}

/** Typeclass that contains JVM fully qualified class name corresponding to given type. `JavaClassName.of[T]` is always
  * equal to `classTag[T].runtimeClass.getName`
  *
  * `JavaClassName` can be used instead of `ClassTag` in ScalaJS when ScalaJS linker is configured to drop class names.
  * Also, unlike `ClassTag`, `JavaClassName` contains just a string so it can be easily serialized and deserialized.
  */
class JavaClassName[T](val value: String) extends AnyVal {
  override def toString: String = value
}
object JavaClassName extends JavaClassNameLowPrio {
  def apply[T](implicit ts: JavaClassName[T]): JavaClassName[T] = ts
  def of[T: JavaClassName]: String = JavaClassName[T].value

  implicit val NothingClassName: JavaClassName[Nothing] = new JavaClassName("scala.runtime.Nothing$")
  implicit val NothingArrayClassName: JavaClassName[Array[Nothing]] = new JavaClassName("[Lscala.runtime.Nothing$;")
  implicit val UnitClassName: JavaClassName[Unit] = new JavaClassName("void")
  implicit val BooleanClassName: JavaClassName[Boolean] = new JavaClassName("boolean")
  implicit val ByteClassName: JavaClassName[Byte] = new JavaClassName("byte")
  implicit val ShortClassName: JavaClassName[Short] = new JavaClassName("short")
  implicit val IntClassName: JavaClassName[Int] = new JavaClassName("int")
  implicit val LongClassName: JavaClassName[Long] = new JavaClassName("long")
  implicit val FloatClassName: JavaClassName[Float] = new JavaClassName("float")
  implicit val DoubleClassName: JavaClassName[Double] = new JavaClassName("double")
  implicit val CharClassName: JavaClassName[Char] = new JavaClassName("char")

  implicit def arrayClassName[T: JavaClassName]: JavaClassName[Array[T]] = {
    val elementName = JavaClassName.of[T] match {
      case "void" => "Lscala.runtime.BoxedUnit;"
      case "boolean" => "Z"
      case "byte" => "B"
      case "short" => "S"
      case "int" => "I"
      case "long" => "J"
      case "float" => "F"
      case "double" => "D"
      case "char" => "C"
      case arr if arr.startsWith("[") => arr
      case n => s"L$n;"
    }
    new JavaClassName("[" + elementName)
  }

  implicit val keyCodec: GenKeyCodec[JavaClassName[?]] =
    GenKeyCodec.create[JavaClassName[?]](new JavaClassName(_), _.value)

  implicit val codec: GenCodec[JavaClassName[?]] =
    GenCodec.nonNullSimple[JavaClassName[?]](i => new JavaClassName(i.readString()), (o, ts) => o.writeString(ts.value))
}
trait JavaClassNameLowPrio { this: JavaClassName.type =>
  implicit def materialize[T]: JavaClassName[T] = macro macros.misc.MiscMacros.javaClassName[T]
}
