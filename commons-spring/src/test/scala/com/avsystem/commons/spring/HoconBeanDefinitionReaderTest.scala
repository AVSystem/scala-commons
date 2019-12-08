package com.avsystem.commons
package spring

import java.{util => ju}

import com.typesafe.config.{Config, ConfigFactory}
import org.springframework.beans.factory.support.DefaultListableBeanFactory
import org.springframework.context.support.GenericApplicationContext

import scala.beans.BeanProperty
import org.scalatest.funsuite.AnyFunSuite

class TestBean(val constrInt: Int = 1, val constrString: String = "constrDefault") {
  @BeanProperty var int: Int = _
  @BeanProperty var string: String = _
  @BeanProperty var strIntMap: ju.Map[String, Int] = _
  @BeanProperty var strList: ju.List[String] = _
  @BeanProperty var strSet: ju.Set[String] = _
  @BeanProperty var nestedBean: TestBean = _
  @BeanProperty var config: Config = _
}
object TestBean {
  def create(theInt: Int = -1, theString: String = "factoryDefault"): TestBean =
    new TestBean(theInt, theString)
}

class HoconBeanDefinitionReaderTest extends AnyFunSuite {
  def createContext(resource: String): GenericApplicationContext = {
    val pnd = new ScalaParameterNameDiscoverer

    val beanFactory = new DefaultListableBeanFactory
    beanFactory.setParameterNameDiscoverer(pnd)

    val ctx = new GenericApplicationContext(beanFactory)
    ctx.addBeanFactoryPostProcessor(new ScalaDefaultValuesInjector)

    val rdr = new HoconBeanDefinitionReader(ctx)
    rdr.loadBeanDefinitions(resource)
    ctx.refresh()

    ctx
  }

  test("hocon bean definition reader should work") {
    val ctx = createContext("testBean.conf")

    val testBean = ctx.getBean("testBean", classOf[TestBean])
    assert(42 == testBean.constrInt)
    assert("lolzsy" == testBean.constrString)
    assert(5 == testBean.int)
    assert("lol" == testBean.string)
    assert(Map("fuu" -> 42).asJava == testBean.strIntMap)
    assert(List("a", "b").asJava == testBean.strList)
    assert(Set("A", "B").asJava == testBean.strSet)
    assert(6 == testBean.nestedBean.int)
    assert(1 == testBean.nestedBean.constrInt)
    assert("wut" == testBean.nestedBean.constrString)
    assert(2 == testBean.nestedBean.nestedBean.constrInt)
    assert("yes" == testBean.nestedBean.nestedBean.constrString)
    assert(ConfigFactory.parseString("srsly = dafuq") == testBean.config)

    val testBeanDefInt = ctx.getBean("testBeanDefInt", classOf[TestBean])
    assert(testBeanDefInt.constrInt == 1)
    assert(testBeanDefInt.constrString == "constrNonDefault")

    val testBeanDefString = ctx.getBean("testBeanDefString", classOf[TestBean])
    assert(testBeanDefString.constrInt == 2)
    assert(testBeanDefString.constrString == "constrDefault")

    val testBeanDefAll = ctx.getBean("testBeanDefAll", classOf[TestBean])
    assert(testBeanDefAll.constrInt == 1)
    assert(testBeanDefAll.constrString == "constrDefault")

    val testBeanFMDefInt = ctx.getBean("testBeanFMDefInt", classOf[TestBean])
    assert(testBeanFMDefInt.constrInt == -1)
    assert(testBeanFMDefInt.constrString == "factoryNonDefault")

    val testBeanFMDefString = ctx.getBean("testBeanFMDefString", classOf[TestBean])
    assert(testBeanFMDefString.constrInt == -2)
    assert(testBeanFMDefString.constrString == "factoryDefault")

    val testBeanFMDefAll = ctx.getBean("testBeanFMDefAll", classOf[TestBean])
    assert(testBeanFMDefAll.constrInt == -1)
    assert(testBeanFMDefAll.constrString == "factoryDefault")
  }
}
