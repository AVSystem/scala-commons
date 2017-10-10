package com.avsystem.commons
package serialization

import java.lang.annotation.RetentionPolicy

import com.github.ghik.silencer.silent

import scala.collection.immutable.ListMap

@silent
class JGenCodecTest extends JCodecTestBase {
  test("java collection test (TreeMap)") {
    testWriteReadAndAutoWriteRead[JSortedMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JNavigableMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
    testWriteReadAndAutoWriteRead[JTreeMap[String, Int]](jTreeMap, ListMap("1" -> 1, "2" -> 2, "3" -> 3))
  }

  test("java enum test") {
    testWriteReadAndAutoWriteRead(RetentionPolicy.RUNTIME, "RUNTIME")
    testWriteReadAndAutoWriteRead(RetentionPolicy.SOURCE, "SOURCE")
  }
}
