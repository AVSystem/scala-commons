package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.HasAnnotation

import scala.annotation.RefiningAnnotation
import scala.util.matching.Regex

/**
 * An alternative way to provide default value for case class parameter used during deserialization with `GenCodec`
 * when its field is missing in data being deserialized. Normally, Scala-level default parameter values are picked up,
 * but you may want to use this annotation instead if you don't want to pollute your Scala classes with unintended
 * default parameter values (i.e. you want a default value *only* for deserialization).
 *
 * {{{
 *   case class HasDefault(@whenAbsent("default") str: String)
 *   object HasDefault extends HasGenCodec[HasDefault]
 * }}}
 *
 * If a parameter has both Scala-level default value and is annotated with `@whenAbsent` then value from annotation
 * takes priority. You can use this to have different source-level default value and different default value for
 * deserialization. You can also leverage this to "remove" default value for deserialization:
 *
 * {{{
 *   case class HasNoDefault(@whenAbsent(throw new Exception) str: String = "default")
 *   object HasDefault extends HasGenCodec[HasDefault]
 * }}}
 *
 * NOTE: [[whenAbsent]] also works for method parameters in RPC framework.
 */
class whenAbsent[+T](v: => T) extends RefiningAnnotation {
  def value: T = v
}
object whenAbsent {
  inline def value[T]: T = ${ valueImpl[T] }
  private def valueImpl[T: Type](using quotes: Quotes): Expr[T] = {
    import quotes.reflect.*

    object DefaultValueMethod {
      private val DefaultValueMethodName: Regex = """(.*)\$default\$(\d+)$""".r

      def unapply(s: Symbol): Option[Symbol] = s match {
        case ms if ms.isDefDef =>
          ms.name match {
            case DefaultValueMethodName(actualMethodName: String, idx: String) =>
              val method = actualMethodName match {
                case "$lessinit$greater" =>
                  ms.owner.companionModule.companionClass.primaryConstructor
                case name =>
                  ms.owner.methodMember(name).headOpt.getOrElse {
                    report.errorAndAbort(s"whenAbsent.value macro could not find method '$name' in ${ms.owner.fullName}")
                  }
              }

              method.paramSymss.flatten.lift(idx.toInt - 1)
            case _ => None
          }
        case _ => None
      }
    }

    val owner = Symbol.spliceOwner.owner match {
      case DefaultValueMethod(paramSymbol) => paramSymbol
      case other => other
    }

    owner.typeRef.asType match {
      case '[fieldType] => '{ getValue[T, fieldType] }
    }
  }
  
  inline private def getValue[T, fieldType]: T = inline HasAnnotation.get[whenAbsent[T], fieldType] match {
    case Some(annot) => annot.value
    case None => compiletime.error("whenAbsent.value can only be used inside a parameter annotated with @whenAbsent")
  }
}
