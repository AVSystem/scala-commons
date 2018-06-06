package com.avsystem.commons
package rpc

trait RpcImplicitsProvider {
  /**
    * If you want some more implicits to be visible by RPC macros, make the companion of your raw RPC trait
    * extend [[RpcImplicitsProvider]] (e.g. through [[RpcMetadataCompanion]]), override this `val` with an `object`
    * and put your additional implicits into it.
    */
  val implicits: Any = null
}
