package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.meta.OptionLike
import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.mongo.text.TextSearchLanguage
import org.bson.{BsonType, BsonValue}

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.util.matching.{Regex => SRegex}

trait VanillaQueryOperatorsDsl[T, R] {

  import MongoQueryOperator._

  def format: MongoFormat[T]

  protected def wrapQueryOperators(operators: MongoQueryOperator[T]*): R

  def is(value: T): R = wrapQueryOperators(Eq(value, format))
  def isNot(value: T): R = wrapQueryOperators(Ne(value, format))
  def gt(value: T): R = wrapQueryOperators(Gt(value, format))
  def gte(value: T): R = wrapQueryOperators(Gte(value, format))
  def lt(value: T): R = wrapQueryOperators(Lt(value, format))
  def lte(value: T): R = wrapQueryOperators(Lte(value, format))
  def in(values: Iterable[T]): R = wrapQueryOperators(In(values, format))
  def in(values: T*): R = in(values)
  def nin(values: Iterable[T]): R = wrapQueryOperators(Nin(values, format))
  def nin(values: T*): R = nin(values)
  def exists: R = exists(true)
  def exists(exists: Boolean): R = wrapQueryOperators(Exists(exists))
  def hasType(bsonType: BsonType): R = wrapQueryOperators(Type(bsonType))
  def mod(divisor: Long, remainder: Long): R = wrapQueryOperators(Mod(divisor, remainder))

  def regex(pattern: String, options: OptArg[String] = OptArg.Empty): R = options match {
    case OptArg(options) => wrapQueryOperators(Regex(pattern), Options(options))
    case OptArg.Empty => wrapQueryOperators(Regex(pattern))
  }

  def regex(pattern: Pattern): R =
    regex(pattern.pattern, if(pattern.flags > 0) OptArg(RegexFlag.optionsAsString(pattern.flags)) else OptArg.Empty)

  def regex(pattern: SRegex): R =
    regex(pattern.pattern)

  def text(
    search: String,
    language: OptArg[TextSearchLanguage] = OptArg.Empty,
    caseSensitive: OptArg[Boolean] = OptArg.Empty,
    diacriticSensitive: OptArg[Boolean] = OptArg.Empty
  ): R =
    wrapQueryOperators(Text(search, language.toOpt, caseSensitive.toOpt, diacriticSensitive.toOpt))

  def not(filter: MongoFilter.Creator[T] => MongoOperatorsFilter[T]): R =
    wrapQueryOperators(Not(filter(new MongoFilter.Creator(format))))

  def rawQueryOp(rawOperator: String, bson: BsonValue): R =
    wrapQueryOperators(Raw(rawOperator, bson))
}
object VanillaQueryOperatorsDsl {
  implicit class ForCollection[C[X] <: Iterable[X], T, R](private val dsl: VanillaQueryOperatorsDsl[C[T], R]) extends AnyVal {

    import MongoQueryOperator._

    private def format: MongoFormat[T] = dsl.format.assumeCollection.elementFormat

    def size(size: Int): R = dsl.wrapQueryOperators(Size(size))

    def elemMatch(filter: MongoFilter.Creator[T] => MongoFilter[T]): R =
      dsl.wrapQueryOperators(ElemMatch(filter(new MongoFilter.Creator(format))))

    def all(values: T*): R = all(values)
    def all(values: Iterable[T]): R = dsl.wrapQueryOperators(All(values, format))
  }
}

trait QueryOperatorsDsl[T, R] extends VanillaQueryOperatorsDsl[T, R] {
  def ===(value: T): R = is(value)
  def !==(value: T): R = isNot(value)

  def >(value: T): R = gt(value)
  def >=(value: T): R = gte(value)
  def <(value: T): R = lt(value)
  def <=(value: T): R = lte(value)

  def startsWith(prefix: String): R =
    regex("^" + SRegex.quote(prefix))

  def containsSubstring(infix: String, caseInsensitive: Boolean = false): R =
    regex(SRegex.quote(infix), if(caseInsensitive) OptArg("i") else OptArg.Empty)
}
object QueryOperatorsDsl {
  implicit class ForCollection[C[X] <: Iterable[X], T, R](private val dsl: QueryOperatorsDsl[C[T], R]) extends AnyVal {
    def isEmpty: R = dsl.size(0)
    def contains(value: T): R = dsl.elemMatch(_.is(value))
    def containsAny(values: T*): R = containsAny(values)
    def containsAny(values: Iterable[T]): R = dsl.elemMatch(_.in(values))
    def containsAll(values: T*): R = containsAll(values)
    def containsAll(values: Iterable[T]): R = dsl.all(values)
  }

  implicit def forOptional[O, T, R](dsl: QueryOperatorsDsl[O, R])(implicit optionLike: OptionLike.Aux[O, T]): ForOptional[O, T, R] =
    new ForOptional(dsl)

  class ForOptional[O, T, R](private val dsl: QueryOperatorsDsl[O, R]) extends AnyVal {
    private def optionLike: OptionLike.Aux[O, T] = dsl.format.assumeOptional[T].optionLike

    def isEmpty: R = dsl.is(optionLike.none)
    def isDefined: R = dsl.isNot(optionLike.none)
  }
}

final class RegexFlag(val javaFlag: Int, val char: Char)(implicit enumCtx: EnumCtx) extends AbstractValueEnum
object RegexFlag extends AbstractValueEnumCompanion[RegexFlag] {
  // code based on org.bson.codecs.PatternCodec

  final val GlobalFlag = 256

  final val CanonEq: Value = new RegexFlag(Pattern.CANON_EQ, 'c')
  final val UnixLines: Value = new RegexFlag(Pattern.UNIX_LINES, 'd')
  final val Global: Value = new RegexFlag(GlobalFlag, 'g')
  final val CaseInsensitive: Value = new RegexFlag(Pattern.CASE_INSENSITIVE, 'i')
  final val Multiline: Value = new RegexFlag(Pattern.MULTILINE, 'm')
  final val Dotall: Value = new RegexFlag(Pattern.DOTALL, 's')
  final val Literal: Value = new RegexFlag(Pattern.LITERAL, 't')
  final val UnicodeCase: Value = new RegexFlag(Pattern.UNICODE_CASE, 'u')
  final val Comments: Value = new RegexFlag(Pattern.COMMENTS, 'x')

  def optionsAsString(javaOptions: Int): String = {
    val sb = new JStringBuilder
    @tailrec def loop(flags: Int, idx: Int): String =
      if(idx >= values.size) sb.toString
      else {
        val flag = values(idx)
        if((flags & flag.javaFlag) > 0) {
          sb.append(flag.char)
          loop(flags - flag.javaFlag, idx+1)
        } else {
          loop(flags, idx+1)
        }
      }
    loop(javaOptions, 0)
  }
}
