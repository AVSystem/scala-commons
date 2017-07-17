package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.GeoradiusAttrs._

import scala.collection.immutable.ListMap

/**
  * Author: ghik
  * Created: 12/09/16.
  */
trait GeoApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  final val Key = "Cities"
  final val StoreKey = "{Cities}Stored"
  final val OtherKey = "Other"

  final val Lodz = GeoPoint(19.45598512887954712, 51.75924813957209381)
  final val Cracow = GeoPoint(19.94497865438461304, 50.06465016961141146)
  final val Warsaw = GeoPoint(21.01222962141036987, 52.2296771784184557)
  final val Wroclaw = GeoPoint(17.03853696584701538, 51.10788563492981496)
  final val Poznan = GeoPoint(16.92516535520553589, 52.40637512515712615)
  final val Gdansk = GeoPoint(18.64663928747177124, 54.35202455234454533)

  final val Cities = ListMap(
    "Kraków" -> Cracow,
    "Warszawa" -> Warsaw,
    "Wrocław" -> Wroclaw,
    "Poznań" -> Poznan,
    "Gdańsk" -> Gdansk,
    "Łódź" -> Lodz
  )

  apiTest("GEOADD") {
    geoadd(Key, Nil).assertEquals(0)
    geoadd(Key, Cities.toList).assertEquals(6)
  }

  apiTest("GEOHASH") {
    setup(geoadd(Key, Cities.toList.take(3)))
    geohash(Key, Nil).assert(_.isEmpty)
    geohash(Key, Cities.keys.toList).assertEquals(List(
      "u2yhvcgrxx0".opt,
      "u3qcnhheum0".opt,
      "u3h4exzj480".opt,
      Opt.Empty,
      Opt.Empty,
      Opt.Empty
    ).map(_.map(GeoHash)))
    geohash(OtherKey, Cities.keys.toList).assertEquals(List.fill(Cities.size)(Opt.Empty))
  }

  apiTest("GEOPOS") {
    setup(geoadd(Key, Cities.toList.take(3)))
    geopos(Key, Cities.keys.toList).assertEquals(List(
      Cracow.opt,
      Warsaw.opt,
      Wroclaw.opt,
      Opt.Empty,
      Opt.Empty,
      Opt.Empty
    ))
    geopos(OtherKey, Cities.keys.toList).assertEquals(List.fill(Cities.size)(Opt.Empty))
  }

  apiTest("GEODIST") {
    setup(geoadd(Key, Cities.toList))
    geodist(Key, "Kraków", "Warszawa").assertEquals(252051.6096.opt)
    geodist(Key, "Kraków", "Warszawa", GeoUnit.Km).assertEquals(252.0516.opt)
    geodist(Key, "Kraków", "Nic").assertEquals(Opt.Empty)
    geodist(OtherKey, "Kraków", "Warszawa").assertEquals(Opt.Empty)
  }

  val Names = List(
    "Łódź",
    "Warszawa",
    "Wrocław",
    "Poznań",
    "Kraków",
    "Gdańsk"
  )
  val Distances = List(
    0.0,
    118.7290,
    182.6025,
    187.3482,
    191.5769,
    293.4112
  )
  val Coords = List(
    Lodz,
    Warsaw,
    Wroclaw,
    Poznan,
    Cracow,
    Gdansk
  )
  val Hashes = List(
    3675939619438045L,
    3676555161427275L,
    3675690699196023L,
    3675964217593962L,
    3675468168314803L,
    3687998315644695L
  )

  apiTest("GEORADIUS") {
    setup(geoadd(Key, Cities.toList))
    georadius(Key, Lodz, 150, GeoUnit.Km)
      .assertEquals(List("Łódź", "Warszawa"))
    georadius(Key, Lodz, 300, GeoUnit.Km, sortOrder = SortOrder.Desc, count = 2L)
      .assertEquals(List("Gdańsk", "Kraków"))
    georadius(OtherKey, Lodz, 10, GeoUnit.Km).assertEquals(Nil)

    val WithDistances = (Distances zip Names).map({ case (dist, name) => Withdist(dist, name) })
    val WithCoordsDistances = (Coords zip WithDistances).map({ case (coord, rest) => Withcoord(coord, rest) })
    val WithHashesCoordsDistances = (Hashes zip WithCoordsDistances).map({ case (hash, rest) => Withhash(hash, rest) })

    georadius(Key, Lodz, 300, GeoUnit.Km, sortOrder = SortOrder.Asc)
      .assertEquals(Names)
    georadius(Key, Lodz, 300, GeoUnit.Km, Withdist, sortOrder = SortOrder.Asc)
      .assertEquals(WithDistances)
    georadius(Key, Lodz, 300, GeoUnit.Km, Withcoord + Withdist, sortOrder = SortOrder.Asc)
      .assertEquals(WithCoordsDistances)
    georadius(Key, Lodz, 300, GeoUnit.Km, Withhash + Withcoord + Withdist, sortOrder = SortOrder.Asc)
      .assertEquals(WithHashesCoordsDistances)
  }

  apiTest("GEORADIUS with STORE") {
    setup(geoadd(Key, Cities.toList))

    georadiusStore(Key, Lodz, 300, GeoUnit.Km, StoreKey).assertEquals(Opt(6L))
    georadiusStore(Key, Lodz, 300, GeoUnit.Km, StoreKey, storeDist = true).assertEquals(Opt(6L))
  }

  apiTest("GEORADIUSBYMEMBER") {
    setup(geoadd(Key, Cities.toList))

    georadiusbymember(Key, "Łódź", 150, GeoUnit.Km)
      .assertEquals(List("Łódź", "Warszawa"))
    georadiusbymember(Key, "Łódź", 300, GeoUnit.Km, sortOrder = SortOrder.Desc, count = 2L)
      .assertEquals(List("Gdańsk", "Kraków"))
    georadiusbymember(OtherKey, "Łódź", 10, GeoUnit.Km).assertEquals(Nil)

    val WithDistances = (Distances zip Names).map({ case (dist, name) => Withdist(dist, name) })
    val WithCoordsDistances = (Coords zip WithDistances).map({ case (coord, rest) => Withcoord(coord, rest) })
    val WithHashesCoordsDistances = (Hashes zip WithCoordsDistances).map({ case (hash, rest) => Withhash(hash, rest) })

    georadiusbymember(Key, "Łódź", 300, GeoUnit.Km, sortOrder = SortOrder.Asc)
      .assertEquals(Names)
    georadiusbymember(Key, "Łódź", 300, GeoUnit.Km, Withdist, sortOrder = SortOrder.Asc)
      .assertEquals(WithDistances)
    georadiusbymember(Key, "Łódź", 300, GeoUnit.Km, Withcoord + Withdist, sortOrder = SortOrder.Asc)
      .assertEquals(WithCoordsDistances)
    georadiusbymember(Key, "Łódź", 300, GeoUnit.Km, Withhash + Withcoord + Withdist, sortOrder = SortOrder.Asc)
      .assertEquals(WithHashesCoordsDistances)
  }

  apiTest("GEORADIUSBYMEMBER with STORE") {
    setup(geoadd(Key, Cities.toList))

    georadiusbymemberStore(Key, "Łódź", 300, GeoUnit.Km, StoreKey).assertEquals(Opt(6L))
    georadiusbymemberStore(Key, "Łódź", 300, GeoUnit.Km, StoreKey, storeDist = true).assertEquals(Opt(6L))
  }
}

