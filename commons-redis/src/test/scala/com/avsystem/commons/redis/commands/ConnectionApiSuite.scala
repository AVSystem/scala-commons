package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis.exception.ErrorReplyException
import com.avsystem.commons.redis.{RedisConnectionCommandsSuite, RedisApi}

/**
  * Author: ghik
  * Created: 29/09/16.
  */
trait ConnectionApiSuite extends RedisConnectionCommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("ECHO") {
    echo("lol").assertEquals("lol")
  }

  apiTest("PING") {
    ping.assertEquals(bs"PONG")
  }

  apiTest("SELECT") {
    select(1).get
  }

  apiTest("SWAPDB") {
    swapdb(0, 1).get
  }
}

class AuthenticationTest extends RedisConnectionCommandsSuite {
  override def password = "hassword".opt

  import RedisApi.Batches.StringTyped._

  test("AUTH") {
    get("key").intercept[ErrorReplyException]
    auth("hassword").get
    get("key").assertEquals(Opt.Empty)
  }
}
