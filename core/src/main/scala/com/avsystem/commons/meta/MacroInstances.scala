package com.avsystem.commons
package meta

import com.avsystem.commons.serialization.TypeRepr

import scala.NamedTuple.{AnyNamedTuple, DropNames, withNames}

/**
 * Intermediate factory that creates an `Instances` trait based on provided `Implicits`. Normally, this factory is used
 * as implicit constructor parameter of base classes for companion objects of RPC traits (e.g.
 * `com.avsystem.commons.rest.DefaultRestApiCompanion`) or ADTs (e.g. `com.avsystem.commons.rest.RestDataCompanion`).
 * This all serves to reduce boilerplate associated with companion declarations and makes RPC trait or ADT definitions
 * as concise as possible. It also lets the programmer easily inject additional implicits into macro-materialization of
 * typeclasses aggregated by `Instances` trait.
 *
 * `Instances` is a trait that aggregates multiple macro materialized typeclass instances. There is no fixed interface
 * for `Instances`, its abstract methods are inspected by `MacroInstances.materialize` macro and implemented by
 * inserting appropriate materializer macro invocation for each typeclass. By default it will be assumed that the
 * typeclass has a macro named `materialize` in its companion object so each method will get
 * `<ResultTypeCompanion>.materialize` as its implementation. This may be overridden with
 * [[com.avsystem.commons.meta.MacroInstances.materializeWith materializeWith]] annotation used on the method. This way
 * you can specify both the object which contains the materializer macro and its name.
 *
 * Additionally, all non-implicit parameters of each method in `Instances` trait will be automatically passed to the
 * materializer macro. See [[com.avsystem.commons.serialization.HasGenCodecFromAU HasGenCodecFromAU]] for an example of
 * this mechanism.
 *
 * Example of `Instances`: `com.avsystem.commons.rest.ClientInstances`
 *
 * The `Implicits` type specifies additional implicits that will be automatically imported into macro materialization.
 * `Implicits` is usually a singleton type of an object which contains these implicits. It may also be a tuple -
 * contents of each tuple component will be imported independently. This way you can combine multiple sources of
 * additional implicits. If you don't want to import any additional implicits, simply use `Unit`.
 *
 * If `MacroInstances` is accepted as implicit super constructor parameter of a companion object (which is the typical
 * situation) then `this` reference should be passed as `companion`. This is in order to work around
 * https://github.com/scala/bug/issues/7666. Actual typeclass instances aggregated by `Instances` trait should be
 * extracted into `implicit lazy val` definitions in the companion base class. See e.g.
 * `com.avsystem.commons.rest.RestDataCompanion` for an example of how it's done.
 */
sealed class MacroInstances[Implicits, Instances <: AnyNamedTuple](applyImpl: (Implicits, Any) => Instances) {
  def apply(implicits: Implicits, companion: Any): Instances = applyImpl(implicits, companion)
}

object MacroInstances {
  inline given materialize[I, S <: AnyNamedTuple]: MacroInstances[I, S] =
    MacroInstances[I, S] { (implicits, companion) =>
      import implicits.given
      summonAll[DropNames[S]].asInstanceOf[S]
    }

  inline def summonAll[T <: Tuple]: T = inline compiletime.erasedValue[T] match {
    case _: EmptyTuple => EmptyTuple.asInstanceOf[T]
    case _: (h *: t) => (summonOrPoly[h] *: summonAll[t]).asInstanceOf[T]
  }

  inline def summonOrPoly[T]: T = compiletime.summonFrom {
    case t: T => t
    case _ =>
      inline compiletime.erasedValue[T] match {
        case _: Function0[r] => (() => compiletime.summonInline[r]).asInstanceOf[T]
        case _: Poly[tc] => ([X] => () => compiletime.summonInline[tc[X]]).asInstanceOf[T]
        case _: PolyWithEv[tc, ev] => ([X:ev] => () => compiletime.summonInline[tc[X]]).asInstanceOf[T]
        case _ => throw new IllegalArgumentException("Unsupported type for MacroInstances: " + compiletime.summonInline[TypeRepr[T]])
      }
  }

  type Poly[TC[_]] = [X] => () => TC[X]
  type PolyWithEv[TC[_], Ev[_]] = [X:Ev] => () => TC[X]

//  inline def summonOrPoly[T]: T = ${summonOrPolyImpl[T]}

//  def summonOrPolyImpl[T: Type](using quotes: Quotes):Expr[T] = {
//    import quotes.reflect.*
//    Expr.summon[T].getOrElse{
//      typeReprInfo(TypeRepr.of[T]).dbg
//
//      TypeRepr.of[T] match {
//        case MethodType(_, params, result) =>
//        case PolyType(_, _, _ ) =>
//        case lambda: LambdaType =>
//      }
//      '{ ??? }
//    }
//  }

  //  macroInstances: Tree = {
  //    val resultTpe = c.macroApplication.tpe
  //    val applySig = resultTpe.member(TermName("apply")).typeSignatureIn(resultTpe)
  //    val implicitsTpe = applySig.paramLists.head.head.typeSignature
  //    val instancesTpe = applySig.finalResultType
  //
  //    val instTs = instancesTpe.typeSymbol
  //    if (!(instTs.isClass && instTs.isAbstract)) {
  //      abort(s"Expected trait or abstract class type, got $instancesTpe")
  //    }
  //
  //    val instancesMethods = instancesTpe.members.iterator
  //      .filter(m => m.isAbstract && m.isMethod && !m.asTerm.isSetter)
  //      .map(_.asMethod)
  //      .toList
  //      .reverse
  //
  //    val CompanionParamName = c.freshName(TermName("companion"))
  //
  //    def impl(singleMethod: Option[Symbol]): Tree = {
  //      val impls = instancesMethods.map { m =>
  //        val sig = m.typeSignatureIn(instancesTpe)
  //        val resultTpe = sig.finalResultType.dealias
  //
  //        val materializer =
  //          if (singleMethod.exists(_ != m))
  //            q"$PredefObj.???"
  //          else
  //            findAnnotation(m, MaterializeWithAT) match {
  //              case Some(annot) =>
  //                val errorPos = annot.errorPos.getOrElse(c.enclosingPosition)
  //                annot.tree match {
  //                  case Apply(_, List(prefix, macroNameTree)) =>
  //                    val macroName = macroNameTree match {
  //                      case StringLiteral(name) => name
  //                      case t if t.symbol.isSynthetic && t.symbol.name.decodedName == TermName("<init>$default$2") =>
  //                        "materialize"
  //                      case _ => abortAt("expected string literal as second argument of @materializeWith", errorPos)
  //                    }
  //                    q"$prefix.${TermName(macroName)}"
  //                  case _ =>
  //                    abortAt("bad @materializeWith annotation", errorPos)
  //                }
  //              case None =>
  //                val resultCompanion = typedCompanionOf(resultTpe).getOrElse(
  //                  abort(s"$resultTpe has no companion object with `materialize` macro")
  //                )
  //                q"$resultCompanion.materialize"
  //            }
  //
  //        val instTpeTree = treeForType(sig.finalResultType)
  //        if (!m.isGetter) {
  //          val tparamDefs = sig.typeParams.map(typeSymbolToTypeDef(_, forMethod = true))
  //          val paramDefs = sig.paramLists.map(_.map(paramSymbolToValDef))
  //          val argss = sig.paramLists match {
  //            case List(Nil) => Nil
  //            case paramss => paramss.filterNot(_.exists(_.isImplicit)).map(_.map(s => q"${s.name.toTermName}"))
  //          }
  //          q"def ${m.name}[..$tparamDefs](...$paramDefs): $instTpeTree = $materializer(...$argss)"
  //        } else if (m.isVar || m.setter != NoSymbol)
  //          q"var ${m.name}: $instTpeTree = $materializer"
  //        else
  //          q"val ${m.name}: $instTpeTree = $materializer"
  //      }
  //
  //      val implicitsName = c.freshName(TermName("implicits"))
  //      def implicitImports(tpe: Type, expr: Tree): List[Tree] = {
  //        val dtpe = tpe.dealias
  //        if (dtpe =:= typeOf[Unit]) Nil
  //        else if (definitions.TupleClass.seq.contains(dtpe.typeSymbol))
  //          dtpe.typeArgs.zipWithIndex.flatMap { case (ctpe, idx) =>
  //            implicitImports(ctpe, q"$expr.${TermName(s"_${idx + 1}")}")
  //          }
  //        else List(q"import $expr._")
  //      }
  //
  //      q"""
  //        new $resultTpe {
  //          def apply($implicitsName: $implicitsTpe, $CompanionParamName: Any): $instancesTpe = {
  //            ..${implicitImports(implicitsTpe, Ident(implicitsName))}
  //            new $instancesTpe { ..$impls; () }
  //          }
  //        }
  //       """
  //    }

  /**
   * Annotation which may be applied on methods of `Implicits` trait in [[MacroInstances]] to instruct
   * [[MacroInstances.materialize]] macro how to implement these methods.
   */
  final class materializeWith(prefix: Any, materializer: String = "materialize") extends StaticAnnotation
}
