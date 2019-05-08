package com.avsystem.commons
package serialization

import java.lang.annotation.RetentionPolicy

import scala.collection.immutable.ListMap

class JGenCodecTest extends JCodecTestBase with SimpleIOCodecTest {
  test("java collection test (TreeMap)") {
    testWrite[JSortedMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWrite[JNavigableMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWrite[JTreeMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
  }

  test("java enum test") {
    testWrite(RetentionPolicy.RUNTIME, "RUNTIME")
    testWrite(RetentionPolicy.SOURCE, "SOURCE")
  }
}
