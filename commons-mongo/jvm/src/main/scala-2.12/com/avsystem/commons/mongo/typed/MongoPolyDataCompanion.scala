package com.avsystem.commons
package mongo.typed

// MongoPolyDataCompanion is a 2.13 only feature
// This serves only to avoid writing macro code cross-compiled for Scala 2.12
private trait AbstractMongoPolyDataCompanion {
  trait macroDslExtensions {
    def as[C <: D[T]]: C = sys.error("stub")
  }
}
