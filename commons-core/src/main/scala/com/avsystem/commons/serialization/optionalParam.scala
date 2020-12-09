package com.avsystem.commons
package serialization

/**
  * Can be applied on case class fields (for `GenCodec` generation) and RPC parameters (for RPC interfaces)
  * collected into a map of raw values.
  * Makes the parameter optional, meaning that its type must be an `Option`, `Opt`, `OptArg`, etc. When the value is
  * empty, the resulting JSON object or map of raw values simply lacks the entry corresponding to this parameter.
  */
class optionalParam extends StaticAnnotation
