package com.avsystem.commons
package serialization

import CodecTestData._

trait JCodecTestBase extends AbstractCodecTest {
  val jTreeMap = stringMap(new JTreeMap[String, Int])
}
