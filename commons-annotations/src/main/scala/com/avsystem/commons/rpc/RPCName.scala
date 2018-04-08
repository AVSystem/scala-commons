package com.avsystem.commons
package rpc

import scala.annotation.StaticAnnotation

/**
  * You can use this annotation on overloaded RPC methods to give them unique identifiers for RPC serialization.
  * You can also subclass this annotation provided that you always override the `name` parameter with another
  * constructor parameter.
  */
class RPCName(val name: String) extends StaticAnnotation
