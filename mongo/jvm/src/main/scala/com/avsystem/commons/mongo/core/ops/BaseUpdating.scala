package com.avsystem.commons
package mongo.core.ops

import com.mongodb.client.model.Updates
import org.bson.conversions.Bson

trait BaseUpdating[T] extends Any with KeyValueHandling[T] {
  def set(t: T): Bson = use(t)(Updates.set)
  def setOnInsert(t: T): Bson = use(t)(Updates.setOnInsert(_, _))
  def unset(): Bson = Updates.unset(key)

  def rename(newName: String): Bson = Updates.rename(key, newName)

  def inc(number: JNumber): Bson = Updates.inc(key, number)
  def mul(number: JNumber): Bson = Updates.mul(key, number)
  def min(t: T): Bson = use(t)(Updates.min)
  def max(t: T): Bson = use(t)(Updates.max)

  def currentDate(): Bson = Updates.currentDate(key)
  def currentTimestamp(): Bson = Updates.currentTimestamp(key)

  def bitwiseAnd(int: Int): Bson = Updates.bitwiseAnd(key, int)
  def bitwiseAnd(long: Long): Bson = Updates.bitwiseAnd(key, long)
  def bitwiseOr(int: Int): Bson = Updates.bitwiseOr(key, int)
  def bitwiseOr(long: Long): Bson = Updates.bitwiseOr(key, long)
  def bitwiseXor(int: Int): Bson = Updates.bitwiseXor(key, int)
  def bitwiseXor(long: Long): Bson = Updates.bitwiseXor(key, long)
}
