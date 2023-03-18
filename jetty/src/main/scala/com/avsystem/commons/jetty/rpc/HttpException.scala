package com.avsystem.commons
package jetty.rpc

import java.io.IOException

final class HttpException(val status: Int, val reason: String) extends IOException(s"HttpException($status): $reason")
