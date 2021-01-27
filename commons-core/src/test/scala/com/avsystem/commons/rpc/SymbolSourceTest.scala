package com.avsystem.commons.rpc

import com.avsystem.commons.meta.{SymbolSource, TypedMetadata, multi, reifySource}
import org.scalatest.funsuite.AnyFunSuite

class Dummy {
  def thing(param: Int): String = "fuuu"
}

case class ClassSourceMetadata[T](
  @reifySource source: SymbolSource,
  @encoded @multi @rpcMethodMetadata methods: List[MethodSourceMetadata[_]]
) extends TypedMetadata[T]
object ClassSourceMetadata extends ApiMetadataCompanion[ClassSourceMetadata]

case class MethodSourceMetadata[T](
  @reifySource source: SymbolSource,
  @encoded @multi @rpcParamMetadata params: List[ParamSourceMetadata[_]]
) extends TypedMetadata[T]

case class ParamSourceMetadata[T](
  @reifySource source: SymbolSource
) extends TypedMetadata[T]

class SymbolSourceTest extends AnyFunSuite {
  test("symbol source metadata") {
    val srcMeta = ClassSourceMetadata.materialize[Dummy]

    assert(srcMeta.source.text ==
      """class Dummy {
        |  def thing(param: Int): String = "fuuu"
        |}""".stripMargin)

    assert(srcMeta.methods.map(_.source.text) == List("""def thing(param: Int): String = "fuuu""""))

    assert(srcMeta.methods.map(_.params.map(_.source.text)) == List(List("param: Int")))
  }
}
