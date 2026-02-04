package com.avsystem.commons
package meta

import scala.NamedTuple.{AnyNamedTuple, DropNames}

//it should be deprecated and removed
sealed class MacroInstances[Implicits, Instances <: AnyNamedTuple](applyImpl: (Implicits, Any) => Instances) {
  def apply(implicits: Implicits, companion: Any): Instances = applyImpl(implicits, companion)
}

object MacroInstances {
  inline given materialize[Implicits, Instances <: AnyNamedTuple]: MacroInstances[Implicits, Instances] =
    MacroInstances[Implicits, Instances] { (implicits, companion) =>
      import implicits.given
      materializeInstances[DropNames[Instances]].asInstanceOf[Instances]
    }
  inline def materializeInstances[T <: Tuple]: T = inline compiletime.erasedValue[T] match {
    case _: EmptyTuple => EmptyTuple.asInstanceOf[T]
    case _: (h *: t) =>
      given AllowDerivation[h] = AllowDerivation.create
      (compiletime.summonInline[h] *: materializeInstances[t]).asInstanceOf[T]
  }
  
  /**
   * Annotation which may be applied on methods of `Implicits` trait in [[MacroInstances]] to instruct
   * [[MacroInstances.materialize]] macro how to implement these methods.
   */
  final class materializeWith(prefix: Any, materializer: String = "materialize") extends StaticAnnotation
}
