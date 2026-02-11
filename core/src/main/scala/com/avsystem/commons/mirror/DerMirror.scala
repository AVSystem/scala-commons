package com.avsystem.commons
package mirror

import scala.annotation.{implicitNotFound, tailrec, RefiningAnnotation}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

@implicitNotFound("No DerMirror could be generated.\nDiagnose any issues by calling DerMirror.derived[T] directly")
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
    type MirroredType = T; type MirroredMonoType = T; type MirroredElemTypes <: Tuple
  }
  type SumOf[T] = DerMirror.Sum {
    type MirroredType = T; type MirroredMonoType = T; type MirroredElemTypes <: Tuple
  }
  type SingletonOf[T] = DerMirror.Singleton {
    type MirroredType = T; type MirroredMonoType = T
  }

  type TransparentOf[T] = DerMirror.Transparent {
    type MirroredType = T; type MirroredMonoType = T
  }

  extension [T](m: DerMirror.Of[T]) {
    inline def hasAnnotation[A <: MetaAnnotation]: Boolean = inline getAnnotation[A] match {
      case _: None.type => false
      case _: Some[?] => true
      case other => other.isDefined
    }
    inline def getAnnotation[A <: MetaAnnotation]: Option[A] = ${ getAnnotationImpl[A, m.Metadata] }
  }
  transparent inline given derived[T]: Of[T] = ${ derivedImpl[T] }
  private def getAnnotationImpl[A <: MetaAnnotation: Type, M <: Meta: Type](using quotes: Quotes): Expr[Option[A]] = {
    import quotes.reflect.*
    val metaTpe = TypeRepr.of[M]
    val annotpe = TypeRepr.of[A]

    @tailrec def loop(tpe: TypeRepr): Option[Expr[A]] = tpe match {
      case AnnotatedType(_, annot) if annot.tpe <:< annotpe => Some(annot.asExprOf[A])
      case AnnotatedType(parent, _) => loop(parent)
      case _ => None
    }

    loop(metaTpe).fold(Expr(None))(a => '{ Some(${ a }) })
  }

  private def stringToType(str: String)(using quotes: Quotes): Type[? <: String] = {
    import quotes.reflect.*
    ConstantType(StringConstant(str)).asType.asInstanceOf[Type[? <: String]]
  }

  private def labelOf[T <: AnyKind: Type](using quotes: Quotes): Option[Type[? <: String]] = {
    import quotes.reflect.*
    TypeRepr.of[T].typeSymbol.getAnnotation(TypeRepr.of[name].typeSymbol).map(_.asExprOf[name]).map {
      case '{ new `name`($value) } => stringToType(value.valueOrAbort)
      case other => report.errorAndAbort(s"Unsupported annotation for label: ${other.show}")
    }
  }

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
        Option.when(tpe.isSingleton || tpe <:< TypeRepr.of[Unit]) {
          val valueImpl = tpe match {
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
          val name =
            if (tpe <:< TypeRepr.of[Unit]) "Unit"
            else {
              val comp = symbol.companionClass
              if (comp.exists) comp.name else symbol.name.stripSuffix("$")
            }

          labelOf[T].getOrElse(stringToType(name)) match {
            case '[type mirroredLabel <: String; mirroredLabel] =>
              '{
                new DerMirror.Singleton {
                  type Metadata = meta
                  type MirroredType = T
                  type MirroredLabel = mirroredLabel
                  def value: T = ${ valueImpl.asExprOf[T] }
                }: DerMirror.SingletonOf[T] {
                  type Metadata = meta
                  type MirroredLabel = mirroredLabel
                }
              }
          }
        } orElse Option.when(tpe.typeSymbol.hasAnnotation(TypeRepr.of[transparent].typeSymbol)) {
          tpe.typeSymbol.caseFields.runtimeChecked match {
            case field :: Nil =>
              (
                field.termRef.widen.asType,
                labelOf(using field.typeRef.asType).getOrElse(stringToType(field.name)),
                labelOf(using symbol.typeRef.asType).getOrElse(stringToType(symbol.name)),
              ).runtimeChecked match {
                case ('[fieldType], '[type elemLabel <: String; elemLabel], '[type label <: String; label]) =>
                  '{
                    new Transparent {
                      type Metadata = meta
                      type MirroredType = T
                      type MirroredLabel = label
                      type MirrorElemType = fieldType
                      type MirroredElemTypes = fieldType *: EmptyTuple
                      type MirroredElemLabels = elemLabel *: EmptyTuple

                      def unwrap(value: T): fieldType =
                        ${ '{ value.castTo[T] }.asTerm.select(field).asExprOf[fieldType] }

                      def wrap(v: fieldType): T = ${
                        New(TypeTree.of[T])
                          .select(symbol.primaryConstructor)
                          .appliedTo('{ v.castTo[fieldType] }.asTerm)
                          .asExprOf[T]
                      }
                    }: DerMirror.TransparentOf[T] {
                      type Metadata = meta
                      type MirroredLabel = label
                      type MirrorElemType = fieldType
                      type MirroredElemTypes = fieldType *: EmptyTuple
                      type MirroredElemLabels = elemLabel *: EmptyTuple
                    }
                  }
              }
          }
        } orElse Option.when(tpe <:< TypeRepr.of[AnyVal]) {
          tpe.typeSymbol.caseFields.runtimeChecked match {
            case field :: Nil =>
              (
                field.termRef.widen.asType,
                labelOf(using field.typeRef.asType).getOrElse(stringToType(field.name)),
                labelOf(using symbol.typeRef.asType).getOrElse(stringToType(symbol.name)),
              ).runtimeChecked match {
                case ('[fieldType], '[type elemLabel <: String; elemLabel], '[type label <: String; label]) =>
                  '{
                    new DerMirror.Product {
                      type Metadata = meta
                      type MirroredType = T
                      type MirroredLabel = label
                      type MirroredMonoType = T
                      type MirroredElemTypes = EmptyTuple
                      type MirroredElemLabels = EmptyTuple

                      def fromUnsafeArray(product: Array[Any]): T = ${
                        New(TypeTree.of[T])
                          .select(symbol.primaryConstructor)
                          .appliedTo('{ product(0).asInstanceOf[fieldType] }.asTerm)
                          .asExprOf[T]
                      }
                    }: DerMirror.ProductOf[T] {
                      type Metadata = meta
                      type MirroredLabel = label
                    }
                  }
              }
          }
        } orElse Expr.summon[Mirror.Of[T]].map {
          case '{
                type mirroredLabel <: String
                type mirroredElemTypes <: Tuple
                type mirroredElemLabels <: Tuple

                $m: Mirror.Product {
                  type MirroredLabel = mirroredLabel
                  type MirroredElemTypes = mirroredElemTypes
                  type MirroredElemLabels = mirroredElemLabels
                }
              } =>

            labelOf[T].getOrElse(Type.of[mirroredLabel]) match {
              case '[type label <: String; label] =>
                '{
                  new DerMirror.Product {
                    type Metadata = meta
                    type MirroredType = T
                    type MirroredLabel = label
                    type MirroredMonoType = T
                    type MirroredElemTypes = mirroredElemTypes
                    type MirroredElemLabels = mirroredElemLabels

                    def fromUnsafeArray(product: Array[Any]): MirroredMonoType =
                      $m.fromProduct(Tuple.fromArray(product)).asInstanceOf[MirroredMonoType]

                  }: DerMirror.ProductOf[T] {
                    type Metadata = meta
                    type MirroredLabel = label
                    type MirroredElemTypes = mirroredElemTypes
                    type MirroredElemLabels = mirroredElemLabels
                  }
                }
            }
          case '{
                type mirroredLabel <: String
                type mirroredElemLabels <: Tuple
                type mirroredElemTypes <: Tuple

                $m: Mirror.SumOf[t] {
                  type MirroredLabel = mirroredLabel
                  type MirroredElemLabels = mirroredElemLabels
                  type MirroredElemTypes = mirroredElemTypes
                }
              } =>
            labelOf[T].getOrElse(Type.of[mirroredLabel]) match {
              case '[type label <: String; label] =>
                '{
                  new DerMirror.Sum {
                    type Metadata = meta
                    type MirroredType = T
                    type MirroredLabel = label
                    type MirroredMonoType = T
                    type MirroredElemTypes = mirroredElemTypes
                    type MirroredElemLabels = mirroredElemLabels
                  }: DerMirror.SumOf[T] {
                    type Metadata = meta
                    type MirroredLabel = label
                    type MirroredElemTypes = mirroredElemTypes
                    type MirroredElemLabels = mirroredElemLabels
                  }
                }
            }
        } getOrElse {
          report.errorAndAbort(s"Unsupported Mirror type for ${tpe.show}")
        }
    }
  }

  sealed trait Product extends DerMirror {
    def fromUnsafeArray(product: Array[Any]): MirroredType
  }
  sealed trait Sum extends DerMirror
  sealed trait Singleton extends DerMirror {
    final type MirroredMonoType = MirroredType
    final type MirroredElemTypes = EmptyTuple
    final type MirroredElemLabels = EmptyTuple
    def value: MirroredType
  }
  sealed trait Transparent extends DerMirror {
    type MirrorElemType
    final type MirroredMonoType = MirroredType
    type MirroredElemTypes <: MirrorElemType *: EmptyTuple
    type MirroredElemLabels <: String *: EmptyTuple
    def unwrap(value: MirroredType): MirrorElemType
    def wrap(value: MirrorElemType): MirroredType
  }

  // workaround for https://github.com/scala/scala3/issues/25245
  extension (value: Any) private def castTo[T]: T = value.asInstanceOf[T]
}
