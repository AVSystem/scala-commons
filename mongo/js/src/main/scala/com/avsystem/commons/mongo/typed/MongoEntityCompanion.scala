package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.serialization.{GenObjectCodec, HasGenObjectCodec}

/** Stub version of JVM's `MongoDataCompanion`. Makes it possible to cross-compile MongoDB entities for Scala.js. Of
  * course, they cannot be used _as_ MongoDB entities in Scala.js - they are simply seen as ADTs with a `GenObjectCodec`
  * instance.
  */
abstract class MongoDataCompanion[T](
  implicit instances: MacroInstances[Unit, () => GenObjectCodec[T]]
) extends HasGenObjectCodec[T]

/** Stub version of JVM's `MongoEntityCompanion`. Makes it possible to cross-compile MongoDB entities for Scala.js. Of
  * course, they cannot be used _as_ MongoDB entities in Scala.js - they are simply seen as ADTs with a `GenObjectCodec`
  * instance.
  */
abstract class MongoEntityCompanion[T <: BaseMongoEntity](
  implicit instances: MacroInstances[Unit, () => GenObjectCodec[T]]
) extends MongoDataCompanion[T]
