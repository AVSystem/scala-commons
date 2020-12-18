package com.avsystem.commons
package di

import com.avsystem.commons.concurrent.RunInQueueEC

import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class DynamicConfig(
  databaseUrl: String,
  bulbulator: BulbulatorConfig
)

case class BulbulatorConfig(
  types: List[String]
)

abstract class MyComponent {
  println(s"starting $this initialization on ${Thread.currentThread().getId}")
  Thread.sleep(100)
  println(s"finished $this initialization")
}

class DynamicDep(db: Database) extends MyComponent

class Database(
  databaseUrl: String
) extends MyComponent

class BulbulatorDao(
  config: BulbulatorConfig
)(implicit
  db: Database,
) extends MyComponent

class DeviceDao(implicit
  db: Database,
) extends MyComponent

class FullApplication(
  dynamicDep: DynamicDep
)(implicit
  bulbulatorDao: BulbulatorDao,
  deviceDao: DeviceDao
) {
  println("full initialization")
}

trait DatabaseComponents extends Components {
  def config: DynamicConfig

  def dynamicDep(db: Database): Component[DynamicDep] =
    component(new DynamicDep(db))

  implicit def database: Component[Database] =
    singleton(new Database(config.databaseUrl))

  implicit def bulbulatorDao: Component[BulbulatorDao] =
    singleton(new BulbulatorDao(config.bulbulator))

  implicit def deviceDao: Component[DeviceDao] =
    singleton(new DeviceDao)
}

class ComponentsExample(val config: DynamicConfig) extends Components with DatabaseComponents {
  def fullApplication: Component[FullApplication] =
    singleton(new FullApplication(dynamicDep(database.ref).ref))
}
object ComponentsExample {

  import ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {
    val config = DynamicConfig("whatever", BulbulatorConfig(List("jeden", "drugi")))
    Await.result(new ComponentsExample(config).fullApplication.init, Duration.Inf)
  }
}
