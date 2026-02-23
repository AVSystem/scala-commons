package com.avsystem.commons
package spring

import java.lang.reflect.{Constructor, Method, Modifier}
import org.springframework.beans.factory.config.ConstructorArgumentValues.ValueHolder
import org.springframework.beans.factory.config.{BeanDefinition, BeanDefinitionHolder, ConfigurableListableBeanFactory}
import org.springframework.beans.factory.support._
import org.springframework.core.{ParameterNameDiscoverer, StandardReflectionParameterNameDiscoverer}

import scala.annotation.tailrec
import scala.beans.BeanProperty
import scala.reflect.{ScalaLongSignature, ScalaSignature}

@deprecated(spring.DeprecatedMessage, "2.27.0")
class ScalaDefaultValuesInjector extends BeanDefinitionRegistryPostProcessor {
  @BeanProperty var paramNameDiscoverer: ParameterNameDiscoverer =
    new StandardReflectionParameterNameDiscoverer

  def classLoader: ClassLoader =
    Thread.currentThread.getContextClassLoader.opt getOrElse getClass.getClassLoader

  def loadClass(name: String): Class[_] = Class.forName(name, false, classLoader)

  def postProcessBeanDefinitionRegistry(registry: BeanDefinitionRegistry): Unit = {
    def traverse(value: Any): Unit = value match {
      case bd: BeanDefinition =>
        bd.getConstructorArgumentValues.getGenericArgumentValues.asScala.foreach(traverse)
        bd.getConstructorArgumentValues.getIndexedArgumentValues.values.asScala.foreach(traverse)
        bd.getPropertyValues.getPropertyValueList.asScala.foreach(pv => traverse(pv.getValue))
        injectDefaultValues(bd)
      case bdw: BeanDefinitionHolder =>
        traverse(bdw.getBeanDefinition)
      case vh: ValueHolder =>
        traverse(vh.getValue)
      case ml: ManagedList[_] =>
        ml.asScala.foreach(traverse)
      case ms: ManagedSet[_] =>
        ms.asScala.foreach(traverse)
      case mm: ManagedMap[_, _] =>
        mm.asScala.foreach { case (k, v) =>
          traverse(k)
          traverse(v)
        }
      case _ =>
    }

    registry.getBeanDefinitionNames.foreach(n => traverse(registry.getBeanDefinition(n)))
  }

  @tailrec
  private def isScalaClass(cls: Class[_]): Boolean = cls.getEnclosingClass match {
    case null =>
      cls.getAnnotation(classOf[ScalaSignature]) != null || cls.getAnnotation(classOf[ScalaLongSignature]) != null
    case encls => isScalaClass(encls)
  }

  private def injectDefaultValues(bd: BeanDefinition): Unit =
    bd.getBeanClassName.opt.map(loadClass).recoverToOpt[ClassNotFoundException].flatten.filter(isScalaClass).foreach {
      clazz =>
        val usingConstructor = bd.getFactoryMethodName == null
        val factoryExecs =
          if (usingConstructor) clazz.getConstructors.toVector
          else clazz.getMethods.iterator.filter(_.getName == bd.getFactoryMethodName).toVector
        val factorySymbolName =
          if (usingConstructor) "$lessinit$greater" else bd.getFactoryMethodName

        if (factoryExecs.size == 1) {
          val constrVals = bd.getConstructorArgumentValues
          val factoryExec = factoryExecs.head
          val paramNames = factoryExec match {
            case c: Constructor[_] => paramNameDiscoverer.getParameterNames(c)
            case m: Method => paramNameDiscoverer.getParameterNames(m)
          }
          (0 until factoryExec.getParameterCount).foreach { i =>
            def defaultValueMethod = clazz
              .getMethod(s"$factorySymbolName$$default$$${i + 1}")
              .recoverToOpt[NoSuchMethodException]
              .filter(m => Modifier.isStatic(m.getModifiers))
            def specifiedNamed = paramNames != null &&
              constrVals.getGenericArgumentValues.asScala.exists(_.getName == paramNames(i))
            def specifiedIndexed =
              constrVals.getIndexedArgumentValues.get(i) != null
            if (!specifiedNamed && !specifiedIndexed) {
              defaultValueMethod.foreach { dvm =>
                constrVals.addIndexedArgumentValue(i, dvm.invoke(null))
              }
            }
          }
        }
    }

}
