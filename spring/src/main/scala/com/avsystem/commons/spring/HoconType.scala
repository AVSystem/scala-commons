package com.avsystem.commons
package spring

import com.typesafe.config._
import scala.annotation.nowarn

@deprecated(spring.DeprecatedMessage, "v2.27.0")
trait HoconType[T] {

  protected def requireNonNull(value: ConfigValue): ConfigValue = {
    require(value != null, s"No value found")
    value
  }

  protected def requireType(requiredType: ConfigValueType, value: ConfigValue): Unit = {
    requireNonNull(value)
    require(
      value.valueType == requiredType,
      s"Value at ${value.origin.description} has type, ${value.valueType}, required $requiredType",
    )
  }

  def get(value: ConfigValue): T
}

@nowarn("msg=deprecated")
object HoconType {

  import com.typesafe.config.ConfigValueType._

  implicit object anyHoconType extends HoconType[Any] {
    def get(value: ConfigValue) =
      requireNonNull(value).unwrapped
  }

  implicit object anyRefHoconType extends HoconType[AnyRef] {
    def get(value: ConfigValue) =
      requireNonNull(value).unwrapped
  }

  implicit object nullHoconType extends HoconType[Null] {
    def get(value: ConfigValue) = {
      requireType(NULL, value)
      null
    }
  }

  implicit object stringHoconType extends HoconType[String] {
    def get(value: ConfigValue) = {
      requireType(STRING, value)
      value.unwrapped.asInstanceOf[String]
    }
  }

  implicit object booleanHoconType extends HoconType[Boolean] {
    def get(value: ConfigValue) = {
      requireType(BOOLEAN, value)
      value.unwrapped.asInstanceOf[Boolean]
    }
  }

  implicit object numberHoconType extends HoconType[JNumber] {
    def get(value: ConfigValue) = {
      requireType(NUMBER, value)
      value.unwrapped.asInstanceOf[JNumber]
    }
  }

  implicit object intHoconType extends HoconType[Int] {
    def get(value: ConfigValue) = {
      requireType(NUMBER, value)
      value.unwrapped.asInstanceOf[JNumber].intValue
    }
  }

  implicit object longHoconType extends HoconType[Long] {
    def get(value: ConfigValue) = {
      requireType(NUMBER, value)
      value.unwrapped.asInstanceOf[JNumber].longValue
    }
  }

  implicit object configHoconType extends HoconType[Config] {
    def get(value: ConfigValue) = {
      requireType(OBJECT, value)
      value.asInstanceOf[ConfigObject].toConfig
    }
  }

  implicit object configValueHoconType extends HoconType[ConfigValue] {
    def get(value: ConfigValue) = value
  }

  implicit object configObjectHoconType extends HoconType[ConfigObject] {
    def get(value: ConfigValue) = {
      requireType(OBJECT, value)
      value.asInstanceOf[ConfigObject]
    }
  }

  implicit object configListHoconType extends HoconType[ConfigList] {
    def get(value: ConfigValue) = {
      requireType(LIST, value)
      value.asInstanceOf[ConfigList]
    }
  }

  implicit def listHoconType[T: HoconType]: HoconType[JList[T]] = new HoconType[JList[T]] {
    def get(value: ConfigValue) = {
      requireType(LIST, value)
      val elementHoconType = implicitly[HoconType[T]]
      value.asInstanceOf[ConfigList].asScala.map(elementHoconType.get).asJava
    }
  }

  implicit def mapHoconType[T: HoconType]: HoconType[JMap[String, T]] = new HoconType[JMap[String, T]] {
    def get(value: ConfigValue) = {
      requireType(OBJECT, value)
      val elementHoconType = implicitly[HoconType[T]]
      value
        .asInstanceOf[ConfigObject]
        .asScala
        .map { case (k, v) =>
          (k, elementHoconType.get(v))
        }
        .asJava
    }
  }

  implicit def optionHoconType[T: HoconType]: HoconType[Option[T]] = new HoconType[Option[T]] {
    def get(value: ConfigValue): Option[T] =
      if (value == null || value.valueType == NULL) None
      else Some(implicitly[HoconType[T]].get(value))
  }

  implicit def eitherHoconType[A: HoconType, B: HoconType]: HoconType[Either[A, B]] = new HoconType[Either[A, B]] {
    def get(value: ConfigValue): Either[A, B] = {
      val leftTry = Try(implicitly[HoconType[A]].get(value))
      val rightTry = Try(implicitly[HoconType[B]].get(value))

      (leftTry, rightTry) match {
        case (Failure(left), Failure(right)) =>
          throw new IllegalArgumentException(
            "Could not parse config value as one of two types:\n" + left.getMessage + "\n" + right.getMessage
          )
        case (Success(left), _) => Left(left)
        case (_, Success(right)) => Right(right)
      }
    }
  }

}
