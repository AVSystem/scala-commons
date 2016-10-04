package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis.exception.ErrorReplyException
import com.avsystem.commons.redis.{RedisConnectionCommandsSuite, RedisStringCommands}

/**
  * Author: ghik
  * Created: 29/09/16.
  */
class ConnectionApiSuite extends RedisConnectionCommandsSuite {

  import RedisStringCommands._

  test("ECHO") {
    echo(bs"lol").assertEquals(bs"lol")
  }

  test("PING") {
    ping.assertEquals(bs"PONG")
  }

  test("SELECT") {
    select(1).get
  }
}

class AuthenticationTest extends RedisConnectionCommandsSuite {
  override def password = "hassword".opt

  import RedisStringCommands._

  test("AUTH") {
    get("key").intercept[ErrorReplyException]
    auth("hassword").get
    get("key").assertEquals(Opt.Empty)
  }
}
