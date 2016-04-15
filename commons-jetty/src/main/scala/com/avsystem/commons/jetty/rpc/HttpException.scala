package com.avsystem.commons
package jetty.rpc

import java.io.IOException

class HttpException(val status: Int) extends IOException(s"HttpException($status)")
