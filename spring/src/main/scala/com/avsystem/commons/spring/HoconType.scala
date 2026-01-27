package com.avsystem.commons
package spring

import com.typesafe.config.*

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

object HoconType {

  import com.typesafe.config.ConfigValueType.*

  given HoconType[Any] = new HoconType[Any] {
    def get(value: ConfigValue): Object =
      requireNonNull(value).unwrapped
  }

  given HoconType[AnyRef] = new HoconType[AnyRef] {
    def get(value: ConfigValue): Object =
      requireNonNull(value).unwrapped
  }

  given HoconType[Null] = new HoconType[Null] {
    def get(value: ConfigValue): Null = {
      requireType(NULL, value)
      null
    }
  }

  given HoconType[String] = new HoconType[String] {
    def get(value: ConfigValue): String = {
      requireType(STRING, value)
      value.unwrapped.asInstanceOf[String]
    }
  }

  given HoconType[Boolean] = new HoconType[Boolean] {
    def get(value: ConfigValue): Boolean = {
      requireType(BOOLEAN, value)
      value.unwrapped.asInstanceOf[Boolean]
    }
  }

  given HoconType[JNumber] = new HoconType[JNumber] {
    def get(value: ConfigValue) = {
      requireType(NUMBER, value)
      value.unwrapped.asInstanceOf[JNumber]
    }
  }

  given HoconType[Int] = new HoconType[Int] {
    def get(value: ConfigValue) = {
      requireType(NUMBER, value)
      value.unwrapped.asInstanceOf[JNumber].intValue
    }
  }

  given HoconType[Long] = new HoconType[Long] {
    def get(value: ConfigValue) = {
      requireType(NUMBER, value)
      value.unwrapped.asInstanceOf[JNumber].longValue
    }
  }

  given HoconType[Config] = new HoconType[Config] {
    def get(value: ConfigValue): Config = {
      requireType(OBJECT, value)
      value.asInstanceOf[ConfigObject].toConfig
    }
  }

  given HoconType[ConfigValue] = new HoconType[ConfigValue] {
    def get(value: ConfigValue) = value
  }

  given HoconType[ConfigObject] = new HoconType[ConfigObject] {
    def get(value: ConfigValue): ConfigObject = {
      requireType(OBJECT, value)
      value.asInstanceOf[ConfigObject]
    }
  }

  given HoconType[ConfigList] = new HoconType[ConfigList] {
    def get(value: ConfigValue): ConfigList = {
      requireType(LIST, value)
      value.asInstanceOf[ConfigList]
    }
  }

  given [T: HoconType] => HoconType[JList[T]] = new HoconType[JList[T]] {
    def get(value: ConfigValue) = {
      requireType(LIST, value)
      val elementHoconType = summon[HoconType[T]]
      value.asInstanceOf[ConfigList].asScala.map(elementHoconType.get).asJava
    }
  }

  given [T: HoconType] => HoconType[JMap[String, T]] = new HoconType[JMap[String, T]] {
    def get(value: ConfigValue) = {
      requireType(OBJECT, value)
      val elementHoconType = summon[HoconType[T]]
      value
        .asInstanceOf[ConfigObject]
        .asScala
        .map { case (k, v) =>
          (k, elementHoconType.get(v))
        }
        .asJava
    }
  }

  given [T: HoconType] => HoconType[Option[T]] = new HoconType[Option[T]] {
    def get(value: ConfigValue): Option[T] =
      if (value == null || value.valueType == NULL) None
      else Some(summon[HoconType[T]].get(value))
  }

  given [A: HoconType, B: HoconType] => HoconType[Either[A, B]] = new HoconType[Either[A, B]] {
    def get(value: ConfigValue): Either[A, B] = {
      val leftTry = Try(summon[HoconType[A]].get(value))
      val rightTry = Try(summon[HoconType[B]].get(value))

      (leftTry, rightTry) match {
        case (Failure(left), Failure(right)) =>
          throw new IllegalArgumentException(
            "Could not parse config value as one of two types:\n" + left.getMessage + "\n" + right.getMessage,
          )
        case (Success(left), _) => Left(left)
        case (_, Success(right)) => Right(right)
      }
    }
  }

}
