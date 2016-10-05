package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.exception.ErrorReplyException
import com.avsystem.commons.redis.protocol.{BulkStringMsg, NullBulkStringMsg}

/**
  * Author: ghik
  * Created: 05/10/16.
  */
trait ClusteredScriptingApiSuite extends CommandsSuite {

  import RedisStringCommands._

  object getScript extends RedisScript[Opt[String]] {
    def source = "return redis.call('get', KEYS[1])"
    def decodeResult = {
      case BulkStringMsg(data) => data.utf8String.opt
      case NullBulkStringMsg => Opt.Empty
    }
  }

  test("EVAL") {
    eval(getScript, Seq("key"), Nil).assertEquals(Opt.Empty)
    set("key", "value").get
    eval(getScript, Seq("key"), Nil).assertEquals("value".opt)

    eval("return redis.call('get', KEYS[1])", Seq("key"), Nil) {
      case BulkStringMsg(data) => data.utf8String.opt
      case NullBulkStringMsg => Opt.Empty
    }.assertEquals("value".opt)
  }

  test("EVALSHA") {
    setup(set("key", "value"))
    intercept[ErrorReplyException](throw evalsha(getScript, Seq("key"), Nil).failed.get)
    eval(getScript, Seq("key"), Nil).get
    evalsha(getScript, Seq("key"), Nil).assertEquals("value".opt)
    evalsha(getScript.sha1, Seq("key"), Nil) {
      case BulkStringMsg(data) => data.utf8String.opt
      case NullBulkStringMsg => Opt.Empty
    }.assertEquals("value".opt)
  }

  test("EVALSHA or EVAL") {
    setup(set("key", "value"))
    asyncClusteredCommands.evalshaOrEval(getScript, Seq("key"), Nil).futureValue shouldEqual "value".opt
    asyncClusteredCommands.evalshaOrEval(getScript, Seq("key"), Nil).futureValue shouldEqual "value".opt
  }
}

trait NodeScriptingApiSuite extends ClusteredScriptingApiSuite {

  import RedisStringCommands._

  test("SCRIPT EXISTS") {
    scriptExists(getScript.sha1).assertEquals(Seq(false))
    scriptLoad(getScript).get
    scriptExists(getScript.sha1).assertEquals(Seq(true))
  }

  test("SCRIPT LOAD") {
    scriptLoad(getScript).assertEquals(getScript.sha1)
  }
}

trait ConnectionScriptingApiSuite extends NodeScriptingApiSuite {

  import RedisStringCommands._

  test("SCRIPT DEBUG") {
    scriptDebug(DebugMode.Yes).get
    scriptDebug(DebugMode.Sync).get
    scriptDebug(DebugMode.No).get
  }
}

class RedisClusterScriptingApiSuite extends RedisClusterCommandsSuite with ClusteredScriptingApiSuite
class RedisNodeScriptingApiSuite extends RedisNodeCommandsSuite with NodeScriptingApiSuite
class RedisConnectionScriptingApiSuite extends RedisConnectionCommandsSuite with ConnectionScriptingApiSuite
