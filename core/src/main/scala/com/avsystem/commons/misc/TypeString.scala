package com.avsystem.commons
package misc

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

/**
 * Typeclass that contains string representation of a concrete type. This representation should correctly parse and
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
object TypeString extends TypeStringMacros {
  def of[T: TypeString]: String = TypeString[T].value
  def apply[T](using ts: TypeString[T]): TypeString[T] = ts

  given GenKeyCodec[TypeString[?]] =
    GenKeyCodec.create[TypeString[?]](new TypeString(_), _.value)

  given GenCodec[TypeString[?]] =
    GenCodec.nonNullSimple[TypeString[?]](i => new TypeString(i.readString()), (o, ts) => o.writeString(ts.value))
}

/**
 * Typeclass that contains JVM fully qualified class name corresponding to given type. `JavaClassName.of[T]` is always
 * equal to `classTag[T].runtimeClass.getName`
 *
 * `JavaClassName` can be used instead of `ClassTag` in ScalaJS when ScalaJS linker is configured to drop class names.
 * Also, unlike `ClassTag`, `JavaClassName` contains just a string so it can be easily serialized and deserialized.
 */
class JavaClassName[T](val value: String) extends AnyVal {
  override def toString: String = value
}
object JavaClassName extends JavaClassNameMacros {
  def apply[T](using ts: JavaClassName[T]): JavaClassName[T] = ts
  def of[T: JavaClassName]: String = JavaClassName[T].value

  given JavaClassName[Nothing] = new JavaClassName("scala.runtime.Nothing$")
  given JavaClassName[Array[Nothing]] = new JavaClassName("[Lscala.runtime.Nothing$;")
  given JavaClassName[Unit] = new JavaClassName("void")
  given JavaClassName[Boolean] = new JavaClassName("boolean")
  given JavaClassName[Byte] = new JavaClassName("byte")
  given JavaClassName[Short] = new JavaClassName("short")
  given JavaClassName[Int] = new JavaClassName("int")
  given JavaClassName[Long] = new JavaClassName("long")
  given JavaClassName[Float] = new JavaClassName("float")
  given JavaClassName[Double] = new JavaClassName("double")
  given JavaClassName[Char] = new JavaClassName("char")

  given[T: JavaClassName] => JavaClassName[Array[T]] = {
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

  given GenKeyCodec[JavaClassName[?]] =
    GenKeyCodec.create[JavaClassName[?]](new JavaClassName(_), _.value)

  given GenCodec[JavaClassName[?]] =
    GenCodec.nonNullSimple[JavaClassName[?]](i => new JavaClassName(i.readString()), (o, ts) => o.writeString(ts.value))
}
