package com.avsystem.commons
package spring

import com.typesafe.config.ConfigFactory


object Playground {
  def main(args: Array[String]): Unit = {
    val conf =
      """
        |a = {b += 1}
        |a = ${a}{b += 2}
      """.stripMargin

    println(ConfigFactory.parseString(conf).resolve().root().render())
  }

}
