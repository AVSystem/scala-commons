package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._

trait KeyedFullApiSuite extends CommandsSuite
  with GeoApiSuite
  with KeyedScriptingApiSuite
  with KeyedKeysApiSuite
  with StringsApiSuite
  with HashesApiSuite
  with SortedSetsApiSuite
  with ListsApiSuite
  with SetsApiSuite
  with HyperLogLogApiSuite
  with StreamsApiSuite

trait NodeFullApiSuite extends KeyedFullApiSuite
  with NodeKeysApiSuite
  with ServerApiSuite
  with NodeScriptingApiSuite

trait ConnectionFullApiSuite extends NodeFullApiSuite
  with ConnectionScriptingApiSuite

class RedisClusterCommandsTest extends RedisClusterCommandsSuite with KeyedFullApiSuite
class RedisMasterSlaveCommandsTest extends RedisMasterSlaveCommandsSuite with NodeFullApiSuite
class RedisNodeCommandsTest extends RedisNodeCommandsSuite with NodeFullApiSuite
class RedisConnectionCommandsTest extends RedisConnectionCommandsSuite with ConnectionFullApiSuite
