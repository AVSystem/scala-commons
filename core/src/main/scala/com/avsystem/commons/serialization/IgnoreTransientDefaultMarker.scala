package com.avsystem.commons
package serialization

/** Instructs [[GenCodec]] to <b>ignore</b> the [[transientDefault]] annotation when serializing a case class. This
  * ensures that even if a field's value is the same as its default, it will be <b>included</b> in the serialized
  * representation. Deserialization behavior remains <b>unchanged</b>. If a field is missing from the input, the default
  * value will be used as usual.
  *
  * This marker can be helpful when using the same model class in multiple contexts with different serialization formats
  * that have conflicting requirements for handling default values.
  *
  * @see
  *   [[CustomMarkersOutputWrapper]] for an easy way to add markers to existing [[Output]] implementations
  */
object IgnoreTransientDefaultMarker extends CustomEventMarker[Unit]
