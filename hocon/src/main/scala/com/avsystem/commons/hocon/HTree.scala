package com.avsystem.commons
package hocon

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.serialization.json.JsonStringOutput

import scala.annotation.tailrec

sealed abstract class HTree extends Product {
  def tokens: HTokenRange
  def pos: SourcePos = tokens.pos

  lazy val children: List[HTree] = productIterator.flatMap {
    case child: HTree => Iterator(child)
    case optChild: Opt[HTree @unchecked] => optChild.iterator
    case children: List[HTree @unchecked] => children.iterator
    case _ => Iterator.empty
  }.toList
}
object HTree {
  final case class HSource(toplevel: HToplevel)(val tokens: HTokenRange) extends HTree

  sealed abstract class HValue extends HTree
  final case class HNull()(val tokens: HTokenRange) extends HValue
  final case class HBoolean(value: Boolean)(val tokens: HTokenRange) extends HValue
  final case class HNumber(value: BigDecimal)(val tokens: HTokenRange) extends HValue
  final case class HString(value: String)(val syntax: HStringSyntax, val tokens: HTokenRange)
    extends HValue with HRegularIncludeTarget
  final case class HSubst(path: HPath)(val optional: Boolean, val tokens: HTokenRange) extends HValue
  final case class HConcat(values: List[HValue])(val tokens: HTokenRange) extends HValue

  sealed abstract class HToplevel extends HValue
  final case class HArray(elements: List[HValue])(val tokens: HTokenRange) extends HToplevel
  final case class HObject(stats: List[HStat])(val tokens: HTokenRange) extends HToplevel

  final case class HPath(prefix: Opt[HPath], key: HKey)(val tokens: HTokenRange) extends HTree {
    @tailrec private def collectValue(acc: List[String]): List[String] = prefix match {
      case Opt(prefix) => prefix.collectValue(key.value :: acc)
      case Opt.Empty => key.value :: acc
    }

    lazy val value: List[String] = collectValue(Nil)
  }

  final case class HKey(parts: List[HString])(val tokens: HTokenRange) extends HTree {
    val value: String = parts.iterator.map(_.value).mkString
  }

  sealed abstract class HStat extends HTree
  final case class HInclude(target: HIncludeTarget)(val tokens: HTokenRange) extends HStat
  final case class HField(path: HPath, value: HValue)(val append: Boolean, val tokens: HTokenRange) extends HStat

  sealed trait HIncludeTarget extends HTree
  sealed trait HRegularIncludeTarget extends HIncludeTarget
  final case class HRequiredInclude(target: HRegularIncludeTarget)(val tokens: HTokenRange) extends HIncludeTarget
  final case class HQualifiedInclude(qualifier: HIncludeQualifier, target: HString)(val tokens: HTokenRange)
    extends HRegularIncludeTarget

  final class HIncludeQualifier(implicit enumCtx: EnumCtx) extends AbstractValueEnum
  object HIncludeQualifier extends AbstractValueEnumCompanion[HIncludeQualifier] {
    final val Classpath, File, Url: Value = new HIncludeQualifier
  }

  final class HStringSyntax(implicit enumCtx: EnumCtx) extends AbstractValueEnum
  object HStringSyntax extends AbstractValueEnumCompanion[HStringSyntax] {
    final val Whitespace, Unquoted, Quoted, Multiline: Value = new HStringSyntax
  }

  def repr(tree: HTree): String = {
    val sb = new JStringBuilder
    def reprIn(tree: HTree, indent: Int, withPos: Boolean): Unit = {
      val attrs = tree match {
        case s: HString => s"[${s.syntax}]"
        case s: HSubst if s.optional => s"[optional]"
        case qi: HQualifiedInclude => s"[${qi.qualifier.name.toLowerCase}]"
        case f: HField if f.append => s"[append]"
        case _ => ""
      }

      if (withPos) {
        sb.append(s"<${tree.pos.startLine + 1}:${tree.pos.startColumn + 1}:")
      }
      val prefix = s"${tree.productPrefix}$attrs"
      sb.append(prefix).append("(")

      tree match {
        case HTree.HBoolean(value) => sb.append(value)
        case HTree.HNumber(value) => sb.append(value)
        case HTree.HString(value) => sb.append(JsonStringOutput.write(value))
        case _ =>
          tree.children match {
            case Nil =>
            case single :: Nil =>
              reprIn(single, indent, single.pos != tree.pos)
            case multiple =>
              multiple.foreach { child =>
                sb.append("\n").append(" " * (indent + 1))
                reprIn(child, indent + 1, withPos = true)
              }
              sb.append("\n").append(" " * indent)
          }
      }

      sb.append(")")
      if (withPos) {
        sb.append(s":${tree.pos.endLine + 1}:${tree.pos.endColumn + 1}>")
      }

    }
    reprIn(tree, 0, withPos = true)
    sb.toString
  }
}
