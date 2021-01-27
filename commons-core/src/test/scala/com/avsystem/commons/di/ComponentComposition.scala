package com.avsystem.commons
package di

import scala.concurrent.Await
import scala.concurrent.duration.Duration

abstract class BaseComponent(implicit info: ComponentInfo) {
  println(s"$info init")
}

class SubDao(implicit info: ComponentInfo) extends BaseComponent
class SubService(dao: SubDao)(implicit info: ComponentInfo) extends BaseComponent

class SubSystem extends Components {
  override protected def componentNamePrefix: String = "sub."

  private val dao: Component[SubDao] =
    component(new SubDao)

  val service: Component[SubService] =
    component(new SubService(dao.ref))
}

class Service(subService: SubService)(implicit info: ComponentInfo) extends BaseComponent

class System(subSystem: SubSystem) extends Components {
  val service: Component[Service] =
    component(new Service(subSystem.service.ref))
}

object ComponentComposition {
  def main(args: Array[String]): Unit = {
    val subSystem = new SubSystem
    val system = new System(subSystem)

    import ExecutionContext.Implicits.global
    Await.result(system.service.init, Duration.Inf)
  }
}
