package com.avsystem.commons
package di

case class DynamicConfig(
  databaseUrl: String,
  bulbulator: BulbulatorConfig
)

case class BulbulatorConfig(
  types: List[String]
)

abstract class MyComponent(implicit name: ComponentName) {
  println(s"starting $name initialization on ${Thread.currentThread().getId}")
  Thread.sleep(100)
  println(s"finished $name initialization")
}

class DynamicDep(db: Database)(implicit
  name: ComponentName,
) extends MyComponent

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

  implicit val database: Component[Database] =
    component(new Database(config.databaseUrl))

  implicit val bulbulatorDao: Component[BulbulatorDao] =
    component(new BulbulatorDao(config.bulbulator))

  implicit val deviceDao: Component[DeviceDao] =
    component(new DeviceDao)
}

class ComponentsExample(val config: DynamicConfig) extends Components with DatabaseComponents {
  val fullApplication: Component[FullApplication] =
    component(new FullApplication(dynamicDep(database.ref).ref))
}
object ComponentsExample {

  import ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {
    val config = DynamicConfig("whatever", BulbulatorConfig(List("jeden", "drugi")))
    new ComponentsExample(config).fullApplication.get
  }
}
