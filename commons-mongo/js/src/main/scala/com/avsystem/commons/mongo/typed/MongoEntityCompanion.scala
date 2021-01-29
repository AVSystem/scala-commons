package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.serialization.{GenObjectCodec, HasGenObjectCodec}

abstract class MongoDataCompanion[T](
  implicit instances: MacroInstances[Unit, () => GenObjectCodec[T]]
) extends HasGenObjectCodec[T]

abstract class MongoEntityCompanion[T <: BaseMongoEntity](
  implicit instances: MacroInstances[Unit, () => GenObjectCodec[T]]
) extends MongoDataCompanion[T]
