package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._

trait HyperLogLogApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("PFADD") {
    pfadd("key", "lol", "foo", "bar").assertEquals(true)
  }

  apiTest("PFCOUNT") {
    setup(pfadd("key", "lol", "foo", "bar"))
    pfcount("key").assert(_ > 0)
  }

  apiTest("PFMERGE") {
    setup(pfadd("{key}1", "lol", "foo", "bar"), pfadd("{key}2", "omg", "wtf", "baz"))
    pfmerge("{key}3", "{key}1", "{key}2").get
  }
}
