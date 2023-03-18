package com.avsystem.commons
package testutil

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSGlobal, JSImport}

@js.native
@JSImport("fs", JSImport.Namespace)
object NodeFS extends js.Object {
  def readFileSync(path: String, encoding: String = js.native): String = js.native
  def writeFileSync(path: String, data: String, encoding: String = js.native): Unit = js.native
}

@js.native
@JSImport("path", JSImport.Namespace)
object NodePath extends js.Object {
  val sep: String = js.native
}

@js.native
@JSGlobal("process.env")
object NodeEnv extends js.Object {
  @JSBracketAccess
  def get(name: String): String = js.native
}

object IO {
  private final val ResourcesPath = NodeEnv.get("RESOURCES_DIR")

  private def nativePath(path: String): String =
    ResourcesPath + path.replace("/", NodePath.sep)

  def readTestResource(path: String): String =
    NodeFS.readFileSync(nativePath(path), "UTF-8")

  def writeTestResource(path: String, data: String): Unit =
    NodeFS.writeFileSync(nativePath(path), data, "UTF-8")
}
