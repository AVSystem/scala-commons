package com.avsystem.commons
package collection

import scala.collection.Factory

trait CrossBuilder[-Elem, +To] extends MBuilder[Elem, To]
trait CrossFactory[-A, +C] extends Factory[A, C]