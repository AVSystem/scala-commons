package com.avsystem.commons.mirror

import scala.annotation.{RefiningAnnotation, implicitNotFound}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

@implicitNotFound(
  "No DerMirror could be generated.\nDiagnose any issues by calling DerMirror.derived[T] directly",
)
sealed trait DerMirror {
  type Metadata <: Meta
  type MirroredType
  type MirroredLabel <: String
  type MirroredMonoType
  type MirroredElemLabels <: Tuple
}

private trait Meta

open class MetaAnnotation extends RefiningAnnotation

object DerMirror {
  type Of[T] = DerMirror {
    type MirroredType = T; type MirroredMonoType; type MirroredElemTypes <: Tuple
  }
  type ProductOf[T] = DerMirror.Product {
    type MirroredType = T; type MirroredMonoType; type MirroredElemTypes <: Tuple
  }
  type SumOf[T] = DerMirror.Sum {
    type MirroredType = T; type MirroredMonoType; type MirroredElemTypes <: Tuple
  }

  type SingletonOf[T] = DerMirror.Singleton {
    type MirroredType = T; type MirroredMonoType = T
  }
  transparent inline def derived[T]: Of[T] = ${ derivedImpl[T] }

  private def derivedImpl[T: Type](using quotes: Quotes): Expr[Of[T]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol

    val annotations = symbol.annotations.filter(_.tpe <:< TypeRepr.of[MetaAnnotation])

    val meta = annotations
      .foldRight(TypeRepr.of[Meta])((annot, tpe) => AnnotatedType(tpe, annot))
      .asType

    meta match {
      case '[type meta <: Meta; meta] =>

        Expr
          .summon[Mirror.Of[T]]
          .map {
            case '{
                  type mirroredLabel <: String;
                  type mirroredElemTypes <: Tuple;
                  type mirroredElemLabels <: Tuple;
                  type mirroredMonoType;

                  $m: Mirror.Product {
                    type MirroredLabel = mirroredLabel;
                    type MirroredElemTypes = mirroredElemTypes;
                    type MirroredElemLabels = mirroredElemLabels;
                    type MirroredMonoType = mirroredMonoType
                  }
                } =>
              if (TypeRepr.of[T].isSingleton) {
                // copied from synthesizedValueOf
                val valueImpl = TypeRepr.of[T] match {
                  case ConstantType(c: Constant) =>
                    Literal(c)
                  case tp: TypeRef if tp <:< TypeRepr.of[Unit] =>
                    Literal(UnitConstant())
                  case n: TermRef =>
                    Ref(n.termSymbol)
                  case ts: ThisType =>
                    This(ts.classSymbol.get)
                  case tp =>
                    report.errorAndAbort(s"Unsupported singleton type: ${tp.show}")
                }

                '{
                  new DerMirror.Singleton {
                    type Metadata = meta
                    type MirroredType = T
                    type MirroredLabel = mirroredLabel
                    def value: T = ${ valueImpl.asExprOf[T] }
                  }: DerMirror.SingletonOf[T] {
                    type Metadata = meta
                    type MirroredType = T
                    type MirroredLabel = mirroredLabel
                  }
                }
              } else {
                '{
                  new DerMirror.Product {
                    type Metadata = meta
                    type MirroredType = T
                    type MirroredLabel = mirroredLabel
                    type MirroredMonoType = mirroredMonoType
                    type MirroredElemTypes = mirroredElemTypes
                    type MirroredElemLabels = mirroredElemLabels
                  }: DerMirror.ProductOf[T] {
                    type Metadata = meta
                    type MirroredType = T
                    type MirroredLabel = mirroredLabel
                    type MirroredMonoType = mirroredMonoType
                    type MirroredElemTypes = mirroredElemTypes
                    type MirroredElemLabels = mirroredElemLabels
                  }
                }
              }
            case '{
                  type mirroredLabel <: String;
                  type mirroredElemLabels <: Tuple;
                  type mirroredElemTypes <: Tuple;
                  type mirroredMonoType;

                  $m: Mirror.SumOf[T] {
                    type MirroredLabel = mirroredLabel;
                    type MirroredElemLabels = mirroredElemLabels;
                    type MirroredElemTypes = mirroredElemTypes;
                    type MirroredMonoType = mirroredMonoType
                  }
                } =>
              '{
                new DerMirror.Sum {
                  type Metadata = meta
                  type MirroredType = T
                  type MirroredLabel = mirroredLabel
                  type MirroredMonoType = mirroredMonoType
                  type MirroredElemTypes = mirroredElemTypes
                  type MirroredElemLabels = mirroredElemLabels
                }: DerMirror.SumOf[T] {
                  type Metadata = meta
                  type MirroredType = T
                  type MirroredLabel = mirroredLabel
                  type MirroredMonoType = mirroredMonoType
                  type MirroredElemTypes = mirroredElemTypes
                  type MirroredElemLabels = mirroredElemLabels
                }
              }
          }
          .getOrElse {
            report.errorAndAbort(s"Unsupported Mirror type for ${tpe.show}")
          }
    }
  }
  sealed trait Product extends DerMirror
  sealed trait Sum extends DerMirror
  sealed trait Singleton extends DerMirror {
    type MirroredMonoType = MirroredType
    type MirroredElemTypes = EmptyTuple
    type MirroredElemLabels = EmptyTuple
    def value: MirroredType
  }
}
