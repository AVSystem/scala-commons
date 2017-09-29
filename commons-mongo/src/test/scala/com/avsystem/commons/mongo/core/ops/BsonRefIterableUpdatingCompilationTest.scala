package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.BsonRef
import com.avsystem.commons.serialization.GenCodec

class BsonRefIterableUpdatingCompilationTest extends BsonRef.Creator[Something] {
  import Updating._

  implicit val codec: GenCodec[Something] = GenCodec.materialize

  new BsonRefIterableUpdating(ref(_.a)) //Instantiation without explicit type parameters
  ref(_.a).push(7)                      //Implicit conversion to BsonRef -> BsonRefIterableUpdating

}

case class Something(a: Set[Int])
