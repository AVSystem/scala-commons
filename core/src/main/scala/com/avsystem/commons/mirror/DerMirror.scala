package com.avsystem.commons
package mirror

import scala.annotation.{RefiningAnnotation, implicitNotFound, tailrec}
import scala.quoted.{Expr, Quotes, Type}

@implicitNotFound("No DerMirror could be generated.\nDiagnose any issues by calling DerMirror.derived directly")
sealed trait DerMirror extends DerElem { outer =>
  final type MirroredElemTypes = Tuple.Map[
    MirroredElems,
    [E] =>> E match {
      case DerElem.Of[t] => t
    },
  ]
  final type MirroredElemLabels = Tuple.Map[
    MirroredElems,
    [E] =>> E match {
      case DerElem.LabelOf[l] => l
    },
  ]
  type MirroredElems <: Tuple
  // maybe some day will be needed
  // def mirroredElems: MirroredElems

  type GeneratedElems <: Tuple
  def generatedElems: GeneratedElems
}

sealed trait DerElem {
  type MirroredType
  type MirroredLabel <: String
  type Metadata <: Meta
}

sealed trait GeneratedDerElem extends DerElem {
  type OuterMirroredType
  def apply(outer: OuterMirroredType): MirroredType
}

// workaround for https://github.com/scala/scala3/issues/25245
sealed trait GeneratedDerElemWorkaround[Outer, Elem] extends GeneratedDerElem {
  final type OuterMirroredType = Outer
  final type MirroredType = Elem
}

object DerElem {
  type Of[T] = DerElem { type MirroredType = T }

  type LabelOf[l <: String] = DerElem { type MirroredLabel = l }
  type MetaOf[m <: Meta] = DerElem { type Metadata = m }
}

private trait Meta

open class MetaAnnotation extends RefiningAnnotation

object DerMirror {
  type Of[T] = DerMirror { type MirroredType = T }
  type ProductOf[T] = DerMirror.Product { type MirroredType = T }
  type SumOf[T] = DerMirror.Sum { type MirroredType = T }
  type SingletonOf[T] = DerMirror.Singleton { type MirroredType = T }
  type TransparentOf[T] = DerMirror.Transparent { type MirroredType = T }

  extension (m: DerMirror) {
    transparent inline def hasAnnotation[A <: MetaAnnotation]: Boolean = ${ hasAnnotationImpl[A, m.type] }
    inline def getAnnotation[A <: MetaAnnotation]: Option[A] = ${ getAnnotationImpl[A, m.type] }
  }
  transparent inline given derived[T]: Of[T] = ${ derivedImpl[T] }
  private def metaOf[DM <: DerMirror: Type](using quotes: Quotes): Type[? <: Meta] =
    Type.of[DM] match {
      // it cannot be extracted via type Metadata = meta
      case '[type meta <: Meta; DerMirror { type Metadata <: meta }] =>
        Type.of[meta]
    }
  private def getAnnotationImpl[A <: MetaAnnotation: Type, DM <: DerMirror: Type](using quotes: Quotes)
    : Expr[Option[A]] = {
    import quotes.reflect.*

    @tailrec def loop(tpe: TypeRepr): Option[Expr[A]] = tpe match {
      case AnnotatedType(underlying, annot) if annot.tpe <:< TypeRepr.of[A] => Some(annot.asExprOf[A])
      case AnnotatedType(underlying, _) => loop(underlying)
      case _ => None
    }

    Expr.ofOption(loop(TypeRepr.of(using metaOf[DM])))
  }
  private def hasAnnotationImpl[A <: MetaAnnotation: Type, DM <: DerMirror: Type](using quotes: Quotes): Expr[Boolean] = {
    import quotes.reflect.*

    @tailrec def loop(tpe: TypeRepr): Boolean = tpe match {
      case AnnotatedType(underlying, annot) if annot.tpe <:< TypeRepr.of[A] => true
      case AnnotatedType(underlying, _) => loop(underlying)
      case _ => false
    }

    Expr(loop(TypeRepr.of(using metaOf[DM])))
  }

  private def stringToType(str: String)(using quotes: Quotes): Type[? <: String] = {
    import quotes.reflect.*
    ConstantType(StringConstant(str)).asType.asInstanceOf[Type[? <: String]]
  }

  private def traverseTypes(tpes: List[Type[? <: AnyKind]])(using Quotes): Type[? <: Tuple] = {
    val empty: Type[? <: Tuple] = Type.of[EmptyTuple]
    tpes.foldRight(empty) {
      case ('[tpe], '[type acc <: Tuple; acc]) => Type.of[tpe *: acc]
      case (_, _) => wontHappen
    }
  }

  private def derivedImpl[T: Type](using quotes: Quotes): Expr[DerMirror.Of[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol

    def metaTypeOf(symbol: Symbol): Type[? <: Meta] = {
      val annotations = symbol.annotations.filter(_.tpe <:< TypeRepr.of[MetaAnnotation])
      annotations
        .foldRight(TypeRepr.of[Meta])((annot, tpe) => AnnotatedType(tpe, annot))
        .asType
        .asInstanceOf[Type[? <: Meta]]
    }

    def labelTypeOf(sym: Symbol, fallback: String): Type[? <: String] =
      stringToType(sym.getAnnotation(TypeRepr.of[name].typeSymbol).map(_.asExprOf[name]) match {
        case Some('{ new `name`($value) }) => value.valueOrAbort
        case _ => fallback
      })

    val generatedElems = for {
        member <- symbol.fieldMembers ++ symbol.declaredMethods
        if member.hasAnnotation(TypeRepr.of[generated].typeSymbol)
        _ = if (!(member.isValDef || member.isDefDef))
          report.errorAndAbort(
            "@generated can only be applied to vals and defs.",
            member.pos.getOrElse(Position.ofMacroExpansion),
          )
        _ = member.paramSymss match {
          case Nil => // no parameters, it's a val or a def without parameters
          case List(Nil) => // a def with empty parameter list
          case paramLists =>
            for {
              paramList <- paramLists
              param <- paramList
            } if (!param.flags.is(Flags.EmptyFlags)) symbolInfo(param).dbg // todo
        }
      } yield {
        val elemTpe = tpe.memberType(member).widen

        (elemTpe.asType, labelTypeOf(member, member.name), metaTypeOf(member)).runtimeChecked match {
          case ('[elemTpe], '[type elemLabel <: String; elemLabel], '[type meta <: Meta; meta]) =>
            '{
              new GeneratedDerElemWorkaround[T, elemTpe] {
                type MirroredLabel = elemLabel
                type Metadata = meta
                def apply(outer: T): elemTpe = ${'{outer}.asTerm.select(member).asExprOf[elemTpe]}
              }: GeneratedDerElem {
                type MirroredType = elemTpe
                type MirroredLabel = elemLabel
                type Metadata = meta
                type OuterMirroredType = T
              }
            }
        }
      }

    def derElemOf(subSymbol: Symbol): Type[? <: DerElem] = {
      val elemTpe =
        if (subSymbol.flags.is(Flags.ParamAccessor)) tpe.memberType(subSymbol)
        else if (subSymbol.isTerm) subSymbol.termRef
        else subSymbol.typeRef

      (elemTpe.asType, labelTypeOf(subSymbol, subSymbol.name), metaTypeOf(subSymbol)).runtimeChecked match {
        case ('[elemTpe], '[type elemLabel <: String; elemLabel], '[type meta <: Meta; meta]) =>
          Type.of[
            DerElem {
              type MirroredType = elemTpe
              type MirroredLabel = elemLabel
              type Metadata = meta
            },
          ]
      }
    }

    def singleCaseFieldOf(symbol: Symbol): Symbol = symbol.caseFields match {
      case field :: Nil => field
      case _ => report.errorAndAbort(s"Expected a single case field for ${symbol.name}")
    }

    def newTFrom(args: List[Expr[?]]): Expr[T] =
      New(TypeTree.of[T])
        .select(symbol.primaryConstructor)
        .appliedToArgs(args.map(_.asTerm))
        .asExprOf[T]

    extension (s: Symbol) {
      def isGenericProduct =
        s.isClassDef && s.flags.is(Flags.Case) && !s.flags.is(Flags.Abstract) &&
          s.primaryConstructor.paramSymss.count(paramList => paramList.isEmpty || !paramList.head.isTypeParam) == 1 &&
          !s.typeRef.derivesFrom(defn.AnyValClass) &&
          !(s.primaryConstructor.flags.is(Flags.Private) || s.primaryConstructor.flags.is(Flags.Protected))

      /**
       * Is this a sealed class or trait for which a sum mirror is generated?
       * It must satisfy the following conditions:
       *   - it has at least one child class or object
       *   - none of its children are anonymous classes
       *   - all of its children are addressable through a path from the parent class
       *     and also the location of the generated mirror.
       *   - all of its children are generic products, singletons, or generic sums themselves.
       */
      def isGenericSum: Boolean =
        s.flags.is(Flags.Sealed) && (s.flags.is(Flags.Abstract) || s.flags.is(Flags.Trait)) && {
          val children = s.children
          children.nonEmpty && children.forall { child =>
            !child.isClassDef || // its a singleton enum value
            child.isGenericProduct || child.isGenericSum
          }
        }
    }

    (
      metaTypeOf(symbol),
      labelTypeOf(symbol, symbol.name.stripSuffix("$")), // find a better way than stripping $
      Expr.ofTupleFromSeq(generatedElems),
    ).runtimeChecked match {
      case (
            '[type meta <: Meta; meta],
            '[type label <: String; label],
            '{type generatedElems <: Tuple; $generatedElemsExpr : generatedElems},
          ) =>
        def deriveSingleton = Option.when(tpe.isSingleton || tpe <:< TypeRepr.of[Unit]) {
          val valueImpl = tpe match {
            case ConstantType(c: Constant) => Literal(c)
            case tp: TypeRef if tp <:< TypeRepr.of[Unit] => Literal(UnitConstant())
            case n: TermRef => Ref(n.termSymbol)
            case ts: ThisType => This(ts.classSymbol.get)
            case tp => report.errorAndAbort(s"Unsupported singleton type: ${tp.show}")
          }
          '{
            new DerMirror.Singleton {
              type MirroredType = T
              type MirroredLabel = label
              type Metadata = meta
              type GeneratedElems = generatedElems

              def generatedElems: GeneratedElems = $generatedElemsExpr
              def value: T = ${ valueImpl.asExprOf[T] }
            }: DerMirror.SingletonOf[T] {
              type MirroredLabel = label
              type Metadata = meta
              type GeneratedElems = generatedElems
            }
          }
        }

        def deriveTransparent = Option.when(symbol.hasAnnotation(TypeRepr.of[transparent].typeSymbol)) {
          if (generatedElems.nonEmpty) report.errorAndAbort("@generated members are not supported in transparent mirrors", symbol.pos.getOrElse(Position.ofMacroExpansion))

          val field = singleCaseFieldOf(symbol)
          (field.termRef.widen.asType, labelTypeOf(field, field.name), metaTypeOf(field)).runtimeChecked match {
            case ('[fieldType], '[type elemLabel <: String; elemLabel], '[type fieldMeta <: Meta; fieldMeta]) =>
              '{
                new TransparentWorkaround[T, fieldType] {
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElems = DerElem {
                    type MirroredType = fieldType
                    type MirroredLabel = elemLabel
                    type Metadata = fieldMeta
                  } *: EmptyTuple

                  def unwrap(value: T): fieldType =
                    ${ '{ value }.asTerm.select(field).asExprOf[fieldType] }

                  def wrap(v: fieldType): T =
                    ${ newTFrom(List('{ v })) }

                }: DerMirror.TransparentOf[T] {
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElemType = fieldType
                  type MirroredElems = DerElem {
                    type MirroredType = fieldType
                    type MirroredLabel = elemLabel
                    type Metadata = fieldMeta
                  } *: EmptyTuple
                }
              }
          }
        }

        def deriveValueClass = Option.when(tpe <:< TypeRepr.of[AnyVal]) {
          val field = singleCaseFieldOf(symbol)
          (field.termRef.widen.asType, labelTypeOf(field, field.name), metaTypeOf(field)).runtimeChecked match {
            case ('[fieldType], '[type elemLabel <: String; elemLabel], '[type fieldMeta <: Meta; fieldMeta]) =>
              '{
                new DerMirror.Product {
                  type MirroredLabel = label
                  type MirroredType = T
                  type Metadata = meta
                  type MirroredElems = DerMirror {
                    type MirroredType = fieldType
                    type MirroredLabel = elemLabel
                    type Metadata = fieldMeta
                  } *: EmptyTuple

                  def fromUnsafeArray(product: Array[Any]): T =
                    ${ newTFrom(List('{ product(0).asInstanceOf[fieldType] })) }

                  type GeneratedElems = generatedElems
                  def generatedElems: GeneratedElems = $generatedElemsExpr
                }: DerMirror.ProductOf[T] {
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElems = DerMirror {
                    type MirroredType = fieldType
                    type MirroredLabel = elemLabel
                    type Metadata = fieldMeta
                  } *: EmptyTuple
                  type GeneratedElems = generatedElems
                }
              }
          }
        }

        def deriveProduct = tpe.classSymbol.filter(_.isGenericProduct).map { cls =>
          val elems = cls.caseFields.map(derElemOf)

          traverseTypes(elems) match {
            case '[type mirroredElems <: Tuple; mirroredElems] =>
              '{
                new DerMirror.Product {
                  type MirroredType = T
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElems = mirroredElems
                  def fromUnsafeArray(product: Array[Any]): T = ???
//    ${
//                    newTFrom(
//                      elems.zipWithIndex.map {
//                        case ('[DerMirror.Of[elemTpe]], idx) => '{ product(${ Expr(idx) }).asInstanceOf[elemTpe] }
//                        case (_, _) => wontHappen
//                      },
//                    )
//                  }

                  type GeneratedElems = generatedElems
                  def generatedElems: GeneratedElems = $generatedElemsExpr
                }: DerMirror.ProductOf[T] {
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElems = mirroredElems
                  type GeneratedElems = generatedElems
                }
              }
          }
        }

        def deriveSum = tpe.classSymbol.filter(_.isGenericSum).map { cls =>
          val elems = cls.children.map(derElemOf)

          traverseTypes(elems) match {
            case '[type mirroredElems <: Tuple; mirroredElems] => {
              '{
                new DerMirror.Sum {
                  type MirroredType = T
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElems = mirroredElems

                  type GeneratedElems = generatedElems
                  def generatedElems: GeneratedElems = $generatedElemsExpr
                }: DerMirror.SumOf[T] {
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElems = mirroredElems
                  type GeneratedElems = generatedElems
                }
              }
            }
          }
        }

        deriveSingleton orElse deriveTransparent orElse deriveValueClass orElse deriveProduct orElse deriveSum getOrElse {
          report.errorAndAbort(s"Unsupported Mirror type for ${tpe.show}")
        }
    }
  }

  sealed trait Product extends DerMirror {
    def fromUnsafeArray(product: Array[Any]): MirroredType
  }
  sealed trait Sum extends DerMirror
  sealed trait Singleton extends DerMirror {
    final type MirroredElems = EmptyTuple
    def value: MirroredType
  }
  sealed trait Transparent extends DerMirror {
    type MirroredElemType
    type MirroredElems <: DerElem.Of[MirroredElemType] *: EmptyTuple
    def unwrap(value: MirroredType): MirroredElemType
    def wrap(value: MirroredElemType): MirroredType

    final type GeneratedElems = EmptyTuple
    final def generatedElems: GeneratedElems = EmptyTuple
  }

  // workaround for https://github.com/scala/scala3/issues/25245
  private sealed trait TransparentWorkaround[T, U] extends DerMirror.Transparent {
    final type MirroredType = T
    final type MirroredElemType = U

    def unwrap(value: T): U
    def wrap(value: U): T
  }
}
