package com.avsystem.commons
package spring

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.funsuite.AnyFunSuite
import org.springframework.beans.factory.support.DefaultListableBeanFactory
import org.springframework.context.support.GenericApplicationContext
import org.springframework.core.StandardReflectionParameterNameDiscoverer

import java.{util => ju}
import scala.beans.BeanProperty

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

class ConditionalTestBean(int: Int) {

  import ConditionalTestBean.initializedCount

  initializedCount += 1
}
object ConditionalTestBean {
  var initializedCount = 0
}

class HoconBeanDefinitionReaderTest extends AnyFunSuite {
  def createContext(resource: String): GenericApplicationContext = {
    val beanFactory = new DefaultListableBeanFactory
    beanFactory.setParameterNameDiscoverer(new StandardReflectionParameterNameDiscoverer)

    val ctx = new GenericApplicationContext(beanFactory)
    ctx.addBeanFactoryPostProcessor(new ScalaDefaultValuesInjector)

    val rdr = new HoconBeanDefinitionReader(ctx)
    rdr.loadBeanDefinitions(resource)
    ctx.refresh()

    ctx
  }

  test("file should be included with true condition") {
    ConditionalTestBean.initializedCount = 0
    val ctx = createContext("conditionalsEnabled.conf")
    val testBean = ctx.getBean("beanFromConditional", classOf[ConditionalTestBean])
    assert(testBean != null)
    assertResult(1)(ConditionalTestBean.initializedCount)
  }

  test("file should not be included with false condition") {
    ConditionalTestBean.initializedCount = 0
    val ctx = createContext("conditionalsDisabled.conf")
    assert(!ctx.containsBean("beanFromConditional"))
    assertResult(0)(ConditionalTestBean.initializedCount)
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
