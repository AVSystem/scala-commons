package com.avsystem.commons
package di

import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class DynamicConfig(
  databaseUrl: String,
  bulbulator: BulbulatorConfig
)

case class BulbulatorConfig(
  types: List[String]
)

abstract class MyComponent(implicit name: ComponentName) extends Injectable {
  println(s"starting $name initialization")
  Thread.sleep(100)
  println(s"finished $name initialization")
}

class Database(
  databaseUrl: String
)(implicit
  name: ComponentName
) extends MyComponent

class BulbulatorDao(
  config: BulbulatorConfig
)(implicit
  db: Database,
  name: ComponentName
) extends MyComponent

class DeviceDao(implicit
  db: Database,
  name: ComponentName
) extends MyComponent

class FullApplication(implicit
  bulbulatorDao: BulbulatorDao,
  deviceDao: DeviceDao
) {
  println("full initialization")
}

trait DatabaseComponents {
  def config: DynamicConfig

  implicit val database: Component[Database] =
    Component(new Database(config.databaseUrl))

  implicit val bulbulatorDao: Component[BulbulatorDao] =
    Component(new BulbulatorDao(config.bulbulator))

  implicit val deviceDao: Component[DeviceDao] =
    Component(new DeviceDao)
}

class ApplicationContext(val config: DynamicConfig) extends DatabaseComponents {
  val fullApplication: Component[FullApplication] = Component(new FullApplication)
}
object ApplicationContext {

  import ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {
    val config = DynamicConfig("whatever", BulbulatorConfig(List("jeden", "drugi")))
    val fut = new ApplicationContext(config).fullApplication.parallelInit()
    Await.result(fut, Duration.Inf)
  }
}


object DotGen {
  def main(args: Array[String]): Unit = {
    val config = DynamicConfig("whatever", BulbulatorConfig(List("jeden", "drugi")))
    val toplevelComponent = new ApplicationContext(config).fullApplication

    val visited = new MHashSet[String]
    val sb = new StringBuilder

    def loop(comp: Component[_]): Unit =
      if (!visited(comp.name)) {
        visited += comp.name
        comp.dependencies.foreach { dep =>
          sb.append(s"${comp.name} -> ${dep.name};\n")
        }
        comp.dependencies.foreach(loop)
      }

    loop(toplevelComponent)

    println(sb.result())
  }
}