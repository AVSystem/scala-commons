package com.avsystem.commons
package serialization

import java.lang.annotation.RetentionPolicy

import scala.collection.immutable.ListMap

class JGenCodecTest extends JCodecTestBase {
  test("java collection test (TreeMap)") {
    testWriteRead[JSortedMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JNavigableMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteRead[JTreeMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
  }

  test("java enum test") {
    testWriteRead(RetentionPolicy.RUNTIME, "RUNTIME")
    testWriteRead(RetentionPolicy.SOURCE, "SOURCE")
  }
}
