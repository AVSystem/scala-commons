package com.avsystem.commons
package serialization

/**
  * This ugly workaround has been introduced when standard `Option` encoding changed from zero-or-one element list
  * encoding to unwrapped-or-null encoding which effectively disallowed serializing `null` and `Some(null)`.
  * If some [[Input]] implementation still wants to use the list encoding, it may do it by extending this trait.
  * Using `legacyEncodingEnabled`, it may also decide at runtime whether it wants to actually use this encoding or not.
  */
trait LegacyOptionEncodingInput { this: Input =>
  def legacyEncodingEnabled: Boolean
}

/**
  * See [[LegacyOptionEncodingInput]]
  */
trait LegacyOptionEncodingOutput { this: Output =>
  def legacyEncodingEnabled: Boolean
}
