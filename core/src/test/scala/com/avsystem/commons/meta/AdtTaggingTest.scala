package com.avsystem.commons
package meta

import com.avsystem.commons.rpc.{caseTag, paramTag, tagged, RpcTag}
import org.scalatest.funsuite.AnyFunSuite

class AdtTaggingTest extends AnyFunSuite {
  sealed trait AdtTag extends RpcTag
  class Good extends AdtTag
  class Bad extends AdtTag

  case class Info[T](
    @reifyName name: String
  ) extends TypedMetadata[T]

  @paramTag[AdtTag](new Good)
  case class AdtClassInfo[T](
    @multi @tagged[Good] @adtParamMetadata goodParams: List[Info[_]],
    @multi @tagged[Bad] @adtParamMetadata badParams: List[Info[_]],
  ) extends TypedMetadata[T] {
    def goodNames: List[String] = goodParams.map(_.name)
    def badNames: List[String] = badParams.map(_.name)
  }
  object AdtClassInfo extends AdtMetadataCompanion[AdtClassInfo]

  @caseTag[AdtTag](new Good)
  case class AdtHierarchyInfo[T](
    @multi @tagged[Good] @adtCaseMetadata goodCases: List[Info[_]],
    @multi @tagged[Bad] @adtCaseMetadata badCases: List[Info[_]],
  ) extends TypedMetadata[T] {
    def goodNames: List[String] = goodCases.map(_.name)
    def badNames: List[String] = badCases.map(_.name)
  }
  object AdtHierarchyInfo extends AdtMetadataCompanion[AdtHierarchyInfo]

  case class Klass(good: Int, @Bad bad: String, @Good alsoGood: String)

  test("adt param tagging") {
    val klassInfo = AdtClassInfo.materialize[Klass]
    assert(klassInfo.goodNames == List("good", "alsoGood"))
    assert(klassInfo.badNames == List("bad"))
  }

  sealed trait Hierarchy
  class GoodCase extends Hierarchy
  @Bad class BadCase extends Hierarchy
  @Good class AlsoGoodCase extends Hierarchy

  test("adt case tagging") {
    val hierarchyInfo = AdtHierarchyInfo.materialize[Hierarchy]
    assert(hierarchyInfo.goodNames == List("GoodCase", "AlsoGoodCase"))
    assert(hierarchyInfo.badNames == List("BadCase"))
  }
}
