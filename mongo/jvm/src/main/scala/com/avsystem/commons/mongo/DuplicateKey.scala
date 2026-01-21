package com.avsystem.commons
package mongo

import com.mongodb.{DuplicateKeyException, ErrorCategory, MongoException}

/** @author
  *   MKej
  */
object DuplicateKey {
  def unapply(t: Throwable): Option[MongoException] = t match {
    case e: DuplicateKeyException => Some(e)
    case e: MongoException if ErrorCategory.fromErrorCode(e.getCode) == ErrorCategory.DUPLICATE_KEY =>
      Some(e)
    case _ => None
  }
}
