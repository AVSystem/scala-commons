package com.avsystem.commons
package serialization

trait JCodecTestBase extends CodecTestBase {
  val jTreeMap = stringMap(new JTreeMap[String, Int])
}
