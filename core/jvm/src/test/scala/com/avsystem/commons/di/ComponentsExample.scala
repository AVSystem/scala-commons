package com.avsystem.commons
package di

import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class DynamicConfig(
  databaseUrl: String,
  bulbulator: BulbulatorConfig,
)

case class BulbulatorConfig(
  types: List[String]
)

abstract class MyComponent {
  println(s"starting $this initialization on ${Thread.currentThread()}")
  Thread.sleep(100)
  println(s"finished $this initialization")

  def destroy(): Unit = {
    println(s"starting teardown of $this")
    Thread.sleep(100)
    println(s"finished teardown of $this")
  }
}

class DynamicDep(db: Database) extends MyComponent

class Database(
  databaseUrl: String
) extends MyComponent

class BulbulatorDao(
  config: BulbulatorConfig
)(implicit db: Database
) extends MyComponent

class DeviceDao(implicit db: Database) extends MyComponent

class FullApplication(
  dynamicDep: DynamicDep
)(implicit
  bulbulatorDao: BulbulatorDao,
  deviceDao: DeviceDao,
) {
  println("full initialization")
}

trait DatabaseComponents extends Components {
  def config: DynamicConfig

  def dynamicDep(db: Component[Database]): Component[DynamicDep] =
    component(new DynamicDep(db.ref)).destroyWith(_.destroy())

  implicit val database: Component[Database] =
    component(new Database(config.databaseUrl)).destroyWith(_.destroy())

  implicit val bulbulatorDao: Component[BulbulatorDao] =
    component(new BulbulatorDao(config.bulbulator)).destroyWith(_.destroy())

  implicit val deviceDao: Component[DeviceDao] =
    component(new DeviceDao).destroyWith(_.destroy())
}

class ComponentsExample(val config: DynamicConfig) extends Components with DatabaseComponents {
  val fullApplication: Component[FullApplication] =
    component(new FullApplication(dynamicDep(database).ref))
}
object ComponentsExample {

  import ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {
    val config = DynamicConfig("whatever", BulbulatorConfig(List("jeden", "drugi")))
    val comps = new ComponentsExample(config)
    Await.result(comps.fullApplication.init, Duration.Inf)
    Await.result(comps.fullApplication.destroy, Duration.Inf)
  }
}
