package com.avsystem.commons
package rpc.akka.serialization

import java.nio.ByteOrder

/**
  * @author Wojciech Milewski
  */
private[serialization] object ByteOrderImplicits {
  implicit val order: ByteOrder = ByteOrder.LITTLE_ENDIAN
}
