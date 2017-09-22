package com.avsystem.commons
package mongo.core.ops

import java.util.regex.Pattern

import com.google.common.collect.ImmutableList
import com.mongodb.client.model.Filters
import com.mongodb.client.model.geojson.{Geometry, Point}
import org.bson.BsonType
import org.bson.conversions.Bson
import org.mongodb.scala.bson.BsonValue

import scala.util.matching.Regex

trait BaseFiltering[T] extends Any {
  private[core] def key: String
  private[core] def encode(t: T): BsonValue

  private def use(t: T)(f: (String, BsonValue) => Bson): Bson = {
    f(key, encode(t))
  }

  def equal(t: T): Bson = use(t)(Filters.eq)
  def notEqual(t: T): Bson = use(t)(Filters.ne)

  def gt(t: T): Bson = use(t)(Filters.gt)
  def lt(t: T): Bson = use(t)(Filters.lt)
  def gte(t: T): Bson = use(t)(Filters.gte)
  def lte(t: T): Bson = use(t)(Filters.lte)

  def in(ts: T*): Bson = Filters.in(key, ts.map(encode).asJava)
  def nin(ts: T*): Bson = Filters.nin(key, ts.map(encode).asJava)

  def exists(exists: Boolean = true): Bson = Filters.exists(key, exists)

  def ofType(bsonType: BsonType): Bson = Filters.`type`(key, bsonType)
  def ofType(typeName: String): Bson = Filters.`type`(key, typeName)

  def mod(divisor: Long, remainder: Long): Bson = Filters.mod(key, divisor, remainder)

  def regex(re: Regex): Bson = regex(re.pattern)
  def regex(pattern: Pattern): Bson = Filters.regex(key, pattern)
  def regex(patternStr: String): Bson = Filters.regex(key, patternStr)
  def regex(patternStr: String, options: String): Bson = Filters.regex(key, patternStr, options)

  def elemMatch(filter: Bson): Bson = Filters.elemMatch(key, filter)
  def size(size: Int): Bson = Filters.size(key, size)

  def bitsAllClear(bitMask: Long): Bson = Filters.bitsAllClear(key, bitMask)
  def bitsAllSet(bitMask: Long): Bson = Filters.bitsAllSet(key, bitMask)
  def bitsAnyClear(bitMask: Long): Bson = Filters.bitsAnyClear(key, bitMask)
  def bitsAnySet(bitMask: Long): Bson = Filters.bitsAnySet(key, bitMask)

  def geoWithinBson(geometryBson: Bson): Bson = Filters.geoWithin(key, geometryBson)
  def geoWithin(geometry: Geometry): Bson = Filters.geoWithin(key, geometry)
  def geoWithinBox(lowerLeftX: Double, lowerLeftY: Double, upperRightX: Double, upperRightY: Double): Bson = {
    Filters.geoWithinBox(key, lowerLeftX, lowerLeftY, upperRightX, upperRightY)
  }
  def geoWithinPolygon(points: (Double, Double)*): Bson = {
    val javaPoints = points.map {
      case (x, y) => ImmutableList.of(x: JDouble, y: JDouble): JList[JDouble]
    }.asJava
    Filters.geoWithinPolygon(key, javaPoints)
  }
  def geoWithinCenter(x: Double, y: Double, radius: Double): Bson = Filters.geoWithinCenter(key, x, y, radius)
  def geoWithinCenterSphere(x: Double, y: Double, radius: Double): Bson = Filters.geoWithinCenterSphere(key, x, y, radius)

  def geoIntersectsBson(geometryBson: Bson): Bson = Filters.geoIntersects(key, geometryBson)
  def geoIntersects(geometry: Geometry): Bson = Filters.geoIntersects(key, geometry)

  private def jDouble(doubleOpt: Opt[Double]): JDouble = doubleOpt.map(d => d: JDouble).orNull
  private def useMinMax(min: Opt[Double], max: Opt[Double])(f: (JDouble, JDouble) => Bson): Bson = {
    f(jDouble(min), jDouble(max))
  }

  def nearBson(geometryBson: Bson, maxDistance: Opt[Double] = Opt.empty, minDistance: Opt[Double] = Opt.empty): Bson = {
    useMinMax(minDistance, maxDistance)(Filters.near(key, geometryBson, _, _))
  }
  def nearPoint(point: Point, maxDistance: Opt[Double] = Opt.empty, minDistance: Opt[Double] = Opt.empty): Bson = {
    useMinMax(minDistance, maxDistance)(Filters.near(key, point, _, _))
  }
  def nearXY(x: Double, y: Double, maxDistance: Opt[Double] = Opt.empty, minDistance: Opt[Double] = Opt.empty): Bson = {
    useMinMax(minDistance, maxDistance)(Filters.near(key, x, y, _, _))
  }

  def nearSphereBson(geometryBson: Bson, maxDistance: Opt[Double] = Opt.empty, minDistance: Opt[Double] = Opt.empty): Bson = {
    useMinMax(minDistance, maxDistance)(Filters.nearSphere(key, geometryBson, _, _))
  }
  def nearSpherePoint(point: Point, maxDistance: Opt[Double] = Opt.empty, minDistance: Opt[Double] = Opt.empty): Bson = {
    useMinMax(minDistance, maxDistance)(Filters.nearSphere(key, point, _, _))
  }
  def nearSphereXY(x: Double, y: Double, maxDistance: Opt[Double] = Opt.empty, minDistance: Opt[Double] = Opt.empty): Bson = {
    useMinMax(minDistance, maxDistance)(Filters.nearSphere(key, x, y, _, _))
  }
}
