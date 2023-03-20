package com.avsystem.commons
package redis

import java.io.File

import org.apache.commons.io.FileUtils
import org.scalatest.Suite

/**
  * Author: ghik
  * Created: 27/06/16.
  */
trait UsesPreconfiguredMasterSlave extends UsesActorSystem with UsesMasterSlaveServers { this: Suite =>
  def masterName: String = "testmaster"
  def ports: Range = 8000 to 8001
  def sentinelPorts: Range = 28000 to 28002
  def preconfiguredDir: File = new File("preconfiguredMasterSlave")

  protected def prepareDirectory(): Unit =
    FileUtils.copyDirectory(preconfiguredDir, masterSlaveDir)
}
