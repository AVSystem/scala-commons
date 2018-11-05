package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._
import com.avsystem.commons.redis.exception.ErrorReplyException
import com.avsystem.commons.redis.protocol.{BulkStringMsg, NullBulkStringMsg}

/**
  * Author: ghik
  * Created: 05/10/16.
  */
trait KeyedScriptingApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  object getScript extends RedisScript[Opt[String]] {
    def source: String = "return redis.call('get', KEYS[1])"
    def decoder: ReplyDecoder[Opt[String]] = ReplyDecoders.nullBulkOr(ReplyDecoders.bulkUTF8)
  }

  apiTest("EVAL") {
    eval(getScript, Seq("key"), Nil).assertEquals(Opt.Empty)
    set("key", "value").get
    eval(getScript, Seq("key"), Nil).assertEquals("value".opt)

    eval("return redis.call('get', KEYS[1])", Seq("key"), Nil) {
      case BulkStringMsg(data) => data.utf8String.opt
      case NullBulkStringMsg => Opt.Empty
    }.assertEquals("value".opt)
  }

  apiTest("EVALSHA") {
    setup(set("key", "value"))
    intercept[ErrorReplyException](evalsha(getScript, Seq("key"), Nil).get)
    eval(getScript, Seq("key"), Nil).get
    evalsha(getScript, Seq("key"), Nil).assertEquals("value".opt)
    evalsha(getScript.sha1, Seq("key"), Nil) {
      case BulkStringMsg(data) => data.utf8String.opt
      case NullBulkStringMsg => Opt.Empty
    }.assertEquals("value".opt)
  }

  apiTest("EVALSHA or EVAL") {
    setup(set("key", "value"))
    asyncKeyedCommands.evalshaOrEval(getScript, Seq("key"), Nil).futureValue shouldEqual "value".opt
    asyncKeyedCommands.evalshaOrEval(getScript, Seq("key"), Nil).futureValue shouldEqual "value".opt
  }
}

trait NodeScriptingApiSuite extends KeyedScriptingApiSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("SCRIPT EXISTS") {
    scriptExists(Nil).assertEquals(Seq.empty)
    scriptExists(getScript.sha1).assertEquals(false)
    scriptLoad(getScript).get
    scriptExists(getScript.sha1).assertEquals(true)
  }

  apiTest("SCRIPT LOAD") {
    scriptLoad(getScript).assertEquals(getScript.sha1)
  }
}

trait ConnectionScriptingApiSuite extends NodeScriptingApiSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("SCRIPT DEBUG") {
    scriptDebug(DebugMode.Yes).get
    scriptDebug(DebugMode.Sync).get
    scriptDebug(DebugMode.No).get
  }
}
