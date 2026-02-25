package com.avsystem.commons
package spring

import com.typesafe.config.ConfigFactory
import org.springframework.context.support.GenericApplicationContext

import scala.annotation.nowarn

@nowarn("msg=deprecated")
object XmlPlayground {
  def main(args: Array[String]): Unit = {
    val ctx = new GenericApplicationContext
    val reader = new HoconBeanDefinitionReader(ctx)
    reader.loadBeanDefinitions(ConfigFactory.load("bean2").withFallback(ConfigFactory.load("bean1")).resolve())

    ctx.refresh()
  }
}
