package com.avsystem.commons
package spring

import java.{lang => jl, util => ju}

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.FunSuite
import org.springframework.beans.factory.support.DefaultListableBeanFactory
import org.springframework.context.support.GenericApplicationContext
import org.springframework.core.DefaultParameterNameDiscoverer

import scala.beans.BeanProperty

class TestBean @ParamNames(Array("constrInt", "constrString"))(val constrInt: Int, val constrString: String) {
  @BeanProperty var int: Int = _
  @BeanProperty var string: String = _
  @BeanProperty var strIntMap: ju.Map[String, Int] = _
  @BeanProperty var strList: ju.List[String] = _
  @BeanProperty var strSet: ju.Set[String] = _
  @BeanProperty var nestedBean: TestBean = _
  @BeanProperty var config: Config = _
}

class HoconBeanDefinitionReaderTest extends FunSuite {
  def createContext(resource: String) = {
    val pnd = new DefaultParameterNameDiscoverer
    pnd.addDiscoverer(new AnnotationParameterNameDiscoverer)

    val beanFactory = new DefaultListableBeanFactory
    beanFactory.setParameterNameDiscoverer(pnd)

    val ctx = new GenericApplicationContext(beanFactory)
    val rdr = new HoconBeanDefinitionReader(ctx)
    rdr.loadBeanDefinitions(resource)
    ctx.refresh()

    ctx
  }

  test("hocon bean definition reader should work") {
    val ctx = createContext("testBean.conf")

    import scala.collection.JavaConverters._

    val testBean = ctx.getBean(classOf[TestBean])
    assert(42 === testBean.constrInt)
    assert("lolzsy" === testBean.constrString)
    assert(5 === testBean.int)
    assert("lol" === testBean.string)
    assert(Map("fuu" -> 42).asJava === testBean.strIntMap)
    assert(List("a", "b").asJava === testBean.strList)
    assert(Set("A", "B").asJava === testBean.strSet)
    assert(6 === testBean.nestedBean.int)
    assert(1 === testBean.nestedBean.constrInt)
    assert("wut" === testBean.nestedBean.constrString)
    assert(2 === testBean.nestedBean.nestedBean.constrInt)
    assert("yes" === testBean.nestedBean.nestedBean.constrString)
    assert(ConfigFactory.parseString("srsly = dafuq") === testBean.config)
  }
}
