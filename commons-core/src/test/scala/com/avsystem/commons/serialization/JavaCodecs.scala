package com.avsystem.commons
package serialization

object JavaCodecs {
  implicit val buildablePojoCodec: GenCodec[BuildablePojo] =
    GenCodec.fromJavaBuilder(BuildablePojo.builder())(_.build())
}
