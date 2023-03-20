package com.avsystem.commons
package hocon

import com.avsystem.commons.misc.{CharSubSequence, SealedEnumCompanion}
import com.avsystem.commons.serialization.json.JsonReader

import scala.annotation.tailrec
import scala.collection.AbstractSeq
import scala.util.matching.Regex

final case class HToken(tokenType: HTokenType, idx: Int, pos: SourcePos) {
  def subPos(from: Int, to: Int): SourcePos =
    SourcePos(pos.input, pos.start + from, pos.start + to)

  def text: String = pos.text

  lazy val unquoted: String = tokenType match {
    case HTokenType.Comment =>
      if (text.startsWith("#")) text.drop(1) else text.drop(2)
    case HTokenType.QuotedString =>
      new JsonReader(text).parseString()
    case HTokenType.MultilineString =>
      text.substring(3, text.length - 3)
    case _ =>
      text
  }

  def multiline: Boolean = pos.endLine > pos.startLine

  override def toString = s"$tokenType($text)"
}

final class HTokenRange(val allTokens: IndexedSeq[HToken], val start: Int, val end: Int)
  extends AbstractSeq[HToken] with IndexedSeq[HToken] { //TODO: IndexedSeqOptimized

  require(allTokens.nonEmpty)
  require(start >= 0 && start <= allTokens.length)
  require(end >= 0 && end <= allTokens.length)
  require(end >= start)

  val pos: SourcePos =
    if (start == end) allTokens(start).pos.emptyStartPos else
      allTokens(start).pos join allTokens(end - 1).pos

  def input: SourceFile = pos.input

  def length: Int = end - start
  def apply(idx: Int): HToken =
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    else allTokens(start + idx)

  def join(other: HTokenRange): HTokenRange = {
    require(allTokens eq other.allTokens)
    new HTokenRange(allTokens, math.min(start, other.start), math.max(end, other.end))
  }
}

sealed abstract class HTokenType {
  def pass(input: String, idx: Int): Int
}
object HTokenType extends SealedEnumCompanion[HTokenType] {
  case object Whitespace extends HTokenType {
    override def pass(input: String, idx: Int): Int = {
      @tailrec def loop(idx: Int): Int =
        if (idx >= input.length) idx
        else if (input(idx).isWhitespace) loop(idx + 1)
        else idx
      loop(idx)
    }
  }

  case object Comment extends HTokenType {
    def pass(input: String, idx: Int): Int =
      if (idx >= input.length) idx
      else if (input.charAt(idx) == '#' || input.substring(idx, math.min(idx + 2, input.length)) == "//") {
        val nlIdx = input.indexOf('\n', idx)
        if (nlIdx == -1) input.length else nlIdx
      } else 0
  }

  sealed abstract class Delimiter(val text: String) extends HTokenType {
    def pass(input: String, idx: Int): Int =
      if (input.substring(idx, idx + text.length) == text) idx + text.length else 0
  }

  case object Bof extends HTokenType {
    def pass(input: String, idx: Int): Int = idx
  }
  case object Eof extends HTokenType {
    def pass(input: String, idx: Int): Int = idx
  }

  case object LBrace extends Delimiter("{")
  case object RBrace extends Delimiter("}")
  case object LBracket extends Delimiter("[")
  case object RBracket extends Delimiter("]")
  case object LParen extends Delimiter("(")
  case object RParen extends Delimiter(")")
  case object Colon extends Delimiter(":")
  case object Equals extends Delimiter("=")
  case object PlusEquals extends Delimiter("+=")
  case object Comma extends Delimiter(",")
  case object Dot extends Delimiter(".")
  case object Splice extends Delimiter("$")
  case object Qmark extends Delimiter("?")

  sealed abstract class Patterned(regex: Regex) extends HTokenType {
    def pass(input: String, idx: Int): Int = {
      val matcher = regex.pattern.matcher(CharSubSequence(input, idx, input.length))
      if (matcher.lookingAt()) idx + matcher.end() else idx
    }
  }

  case object Unquoted extends HTokenType {
    final val Forbidden = ".$\"{}[]():=,+#`^?!@*&\\"

    def pass(input: String, idx: Int): Int = {
      @tailrec def loop(idx: Int, afterSlash: Boolean = false): Int =
        if (idx >= input.length) idx
        else {
          val c = input.charAt(idx)
          if (c.isWhitespace || Forbidden.contains(c)) idx
          else if (afterSlash && c == '/') idx - 1
          else loop(idx + 1, c == '/')
        }
      loop(idx)
    }
  }

  case object MultilineString extends Patterned(""""{3}("{0,2}[^"]+)*"{3,}""".r)
  case object QuotedString extends Patterned(""""([^"\n\\]+|\\("|\\|/|b|f|n|r|t|u[0-9A-Fa-f]{4}))*"""".r)

  final val values: List[HTokenType] = caseObjects
}

class HLexer(input: SourceFile) {
  private[this] val chars = input.contents
  private[this] var tokenIdx = 0
  private[this] var off = -1

  def next(): HToken =
    if (off == -1) {
      off = 0
      tokenIdx += 1
      HToken(HTokenType.Bof, tokenIdx, input.pos(off, off))
    }
    else if (off == chars.length) {
      val pos = input.pos(off, off)
      off += 1
      HToken(HTokenType.Eof, tokenIdx, pos)
    }
    else {
      HTokenType.values.iterator.flatMap { tt =>
        val nextOff = tt.pass(chars, off)
        if (nextOff <= off) Opt.Empty else Opt {
          val token = HToken(tt, tokenIdx, input.pos(off, nextOff))
          off = nextOff
          tokenIdx += 1
          token
        }
      }.nextOpt.getOrElse {
        throw new Exception(s"Unexpected character ${chars.charAt(off)} at ${input.pos(off, off)}")
      }
    }

  def tokenize(): IndexedSeq[HToken] = {
    val res = IArraySeq.newBuilder[HToken]
    while (off <= chars.length) {
      res += next()
    }
    res.result()
  }
}
