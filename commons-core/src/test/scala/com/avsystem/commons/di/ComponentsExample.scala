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

abstract class MyComponent(implicit name: ComponentName) {
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

trait DatabaseComponents extends Components {
  def config: DynamicConfig

  implicit val database: Component[Database] =
    component(new Database(config.databaseUrl))

  implicit val bulbulatorDao: Component[BulbulatorDao] =
    component(new BulbulatorDao(config.bulbulator))

  implicit val deviceDao: Component[DeviceDao] =
    component(new DeviceDao)
}

class ComponentsExample(val config: DynamicConfig) extends Components with DatabaseComponents {
  val fullApplication: Component[FullApplication] =
    component(new FullApplication)
}
object ComponentsExample {

  import ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {
    val config = DynamicConfig("whatever", BulbulatorConfig(List("jeden", "drugi")))
    val fut = new ComponentsExample(config).fullApplication.parallelInit()
    Await.result(fut, Duration.Inf)
  }
}
