package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._

/**
  * Author: ghik
  * Created: 06/10/16.
  */
trait ClusteredFullApiSuite extends CommandsSuite
  with GeoApiSuite
  with ClusteredScriptingApiSuite
  with ClusteredKeysApiSuite
  with StringsApiSuite
  with HashesApiSuite
  with SortedSetsApiSuite

trait NodeFullApiSuite extends ClusteredFullApiSuite
  with NodeKeysApiSuite
  with ServerApiSuite
  with NodeScriptingApiSuite

trait ConnectionFullApiSuite extends NodeFullApiSuite
  with ConnectionScriptingApiSuite

class RedisClusterCommandsTest extends RedisClusterCommandsSuite with ClusteredFullApiSuite
class RedisNodeCommandsTest extends RedisNodeCommandsSuite with NodeFullApiSuite with NodeOnlyServerApiSuite
class RedisConnectionCommandsTest extends RedisConnectionCommandsSuite with ConnectionFullApiSuite
