package com.avsystem.commons
package testutil

import java.io.{BufferedReader, File, FileWriter, InputStreamReader}

import scala.annotation.tailrec

object IO {
  def readTestResource(path: String): String = {
    val reader = new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(path)))
    val buf = new Array[Char](2048)
    @tailrec def loop(sb: JStringBuilder): String = {
      reader.read(buf) match {
        case -1 => sb.toString
        case count => loop(sb.append(buf, 0, count))
      }
    }
    try loop(new JStringBuilder) finally reader.close()
  }

  def writeTestResource(path: String, data: String): Unit = {
    // assuming working dir used by intellij
    val writer = new FileWriter(s"src/test/resources$path".replace("/", File.separator))
    try writer.write(data) finally writer.close()
  }
}
