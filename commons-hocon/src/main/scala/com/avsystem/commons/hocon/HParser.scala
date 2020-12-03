package com.avsystem.commons
package hocon

import java.io.EOFException
import scala.annotation.tailrec

object HParser {
  final val Include = "include"
  final val True = "true"
  final val False = "false"
  final val Null = "null"
  final val Required = "required"

  final val NumberRegex = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r
}
class HParser(tokens: IndexedSeq[HToken]) {

  import HParser._
  import HTokenType._
  import HTree._

  private[this] var idx = 0

  private def fail(): Nothing =
    if (eof) throw new EOFException
    else throw new Exception(s"unexpected token $cur at ${cur.pos}")

  private def cur: HToken = tokens(idx)

  private def prev: HToken = tokens(idx - 1)

  private def eof: Boolean = cur.tokenType == HTokenType.Eof

  private def rangeFrom(start: Int): HTokenRange =
    new HTokenRange(tokens, start, idx)

  private def advance(): Unit = {
    idx += 1
  }

  @tailrec private def skipWs(newlines: Boolean = true): Int =
    if (eof) idx
    else cur.tokenType match {
      case Whitespace | Comment if newlines || !cur.multiline =>
        advance()
        skipWs(newlines)
      case _ =>
        idx
    }

  private def ahead(tt: HTokenType): Boolean =
    ahead(_.tokenType == tt)

  private def aheadAny(tts: HTokenType*): Boolean =
    ahead(t => tts.contains(t.tokenType))

  private def ahead(test: HToken => Boolean): Boolean =
    !eof && test(cur)

  private def pass(tt: HTokenType): Boolean =
    pass(_.tokenType == tt)

  private def passAny(tts: HTokenType*): Boolean =
    pass(t => tts.contains(t.tokenType))

  private def pass(test: HToken => Boolean): Boolean =
    if (!eof && test(cur)) advance().thenReturn(true) else false

  private def passUnquoted(text: String): Boolean =
    pass(t => t.tokenType == Unquoted && t.text == text)

  private def ensure(tt: HTokenType): HToken =
    if (!eof && cur.tokenType == tt) {
      val res = cur
      advance()
      res
    } else fail()

  def parseSource(): HSource = {
    val start = idx
    pass(HTokenType.Bof)
    val afterBof = idx
    skipWs()
    val toplevel = cur.tokenType match {
      case LBrace => parseObject()
      case LBracket => parseArray()
      case _ =>
        idx = afterBof // take whitespaces and comments into the object
        parseObject(braces = false)
    }
    pass(HTokenType.Eof)
    HSource(toplevel)(rangeFrom(start))
  }

  private def parseElems[T](parseElem: () => T, closing: Opt[HTokenType]): List[T] = {
    val elemsBuf = new MListBuffer[T]
    @tailrec def doParseElems(): List[T] = {
      elemsBuf += parseElem()
      skipWs(newlines = false)
      val newline = pass(t => t.tokenType == Whitespace && t.multiline)
      skipWs()
      val comma = pass(Comma)
      skipWs()
      if (eof || closing.contains(cur.tokenType)) elemsBuf.result()
      else if (newline || comma) doParseElems()
      else fail()
    }

    skipWs()
    if (eof || closing.contains(cur.tokenType)) Nil
    else doParseElems()
  }

  def parseObject(braces: Boolean = true): HObject = {
    val start = idx
    if (braces) {
      ensure(LBrace)
    }
    val stats = parseElems(parseStat, if (braces) Opt(RBrace) else Opt.Empty)
    if (braces) {
      ensure(RBrace)
    } else {
      skipWs()
      if (!eof) {
        fail()
      }
    }
    HObject(stats)(rangeFrom(start))
  }

  def parseArray(): HArray = {
    val start = skipWs()
    ensure(LBracket)
    val elems = parseElems(parseValue, Opt(RBracket))
    ensure(RBracket)
    HArray(elems)(rangeFrom(start))
  }

  def parseStat(): HStat = {
    skipWs()
    cur.tokenType match {
      case Unquoted if cur.text == Include => parseInclude()
      case _ => parseField()
    }
  }

  def parseInclude(): HInclude = {
    val start = skipWs()
    passUnquoted(Include)
    val target = parseIncludeTarget()
    HInclude(target)(rangeFrom(start))
  }

  def parseIncludeTarget(): HIncludeTarget = {
    val start = skipWs()
    if (passUnquoted(Required)) {
      ensure(LParen)
      skipWs()
      val regular = parseRegularIncludeTarget()
      skipWs()
      ensure(RParen)
      HRequiredInclude(regular)(rangeFrom(start))
    } else {
      parseRegularIncludeTarget()
    }
  }

  def parseRegularIncludeTarget(): HRegularIncludeTarget = {
    val start = skipWs()
    HIncludeQualifier.values
      .findOpt(q => passUnquoted(q.name.toLowerCase))
      .map { qual =>
        ensure(LParen)
        skipWs()
        val quoted = ensure(QuotedString)
        val range = new HTokenRange(tokens, quoted.idx, quoted.idx + 1)
        val hstring = HString(quoted.unquoted)(HStringSyntax.Quoted, range)
        skipWs()
        ensure(RParen)
        HQualifiedInclude(qual, hstring)(rangeFrom(start))
      }
      .getOrElse {
        val quoted = ensure(QuotedString)
        val range = new HTokenRange(tokens, quoted.idx, quoted.idx + 1)
        HString(quoted.unquoted)(HStringSyntax.Quoted, range)
      }
  }

  def parseField(): HField = {
    val start = skipWs()
    val path = parsePath()
    skipWs()
    val append = cur.tokenType == PlusEquals
    val sepPresent = passAny(Colon, Equals, PlusEquals)
    val value = if (sepPresent) parseValue() else parseObject()
    HField(path, value)(append, rangeFrom(start))
  }

  def parsePath(): HPath = {
    val start = skipWs()
    @tailrec def collectKeys(acc: List[HKey]): List[HKey] = {
      val nextKey = parseKey()
      if (pass(Dot)) collectKeys(nextKey :: acc)
      else nextKey :: acc
    }
    val result = collectKeys(Nil).foldRight(Opt.empty[HPath]) { (key, prefix) =>
      Opt(HPath(prefix, key)(new HTokenRange(tokens, start, key.tokens.end)))
    }
    result.get // at least one key is ensured
  }

  def parseKey(): HKey = {
    skipWs()
    val parts = new MListBuffer[HString]
    parts += parseString(inKey = true)
    while (aheadAny(Whitespace, Unquoted, QuotedString, MultilineString, LParen, RParen)) {
      parts += parseString(inKey = true)
    }
    HKey(parts.result())(new HTokenRange(tokens, parts.head.tokens.start, parts.last.tokens.end))
  }

  def parseValue(): HValue = {
    val start = skipWs()
    val values = new MListBuffer[HValue]
    values += parseNonConcat()
    while (
      aheadAny(LBrace, LBracket, Splice, QuotedString, MultilineString) ||
        ahead(unquotedStringPart(inKey = false))
    ) {
      values += parseNonConcat()
    }
    values.last match {
      case hs: HString if hs.syntax == HStringSyntax.Whitespace =>
        values.remove(values.size - 1)
      case _ =>
    }
    values.result() match {
      case single :: Nil => single
      case multiple => HConcat(multiple)(rangeFrom(start))
    }
  }

  def parseNonConcat(): HValue = {
    if (ahead(LBrace)) parseObject()
    else if (ahead(LBracket)) parseArray()
    else if (ahead(Splice)) parseSubst()
    else {
      val str = parseString(inKey = false)
      if (str.syntax == HStringSyntax.Unquoted) str.value match {
        case Null => HNull()(str.tokens)
        case True => HBoolean(value = true)(str.tokens)
        case False => HBoolean(value = false)(str.tokens)
        case num if NumberRegex.pattern.matcher(num).matches() => HNumber(BigDecimal(num))(str.tokens)
        case _ => str
      }
      else str
    }
  }

  private def unquotedStringPart(inKey: Boolean): HToken => Boolean =
    token => token.tokenType match {
      case Unquoted | LParen | RParen => true
      case Dot => !inKey
      case Whitespace => !token.multiline
      case _ => false
    }

  def parseString(inKey: Boolean): HString = {
    val start = idx
    if (pass(QuotedString)) {
      HString(prev.unquoted)(HStringSyntax.Quoted, rangeFrom(start))
    } else if (pass(MultilineString)) {
      HString(prev.unquoted)(HStringSyntax.Multiline, rangeFrom(start))
    } else if (pass(unquotedStringPart(inKey))) {
      while (ahead(unquotedStringPart(inKey))) {
        advance()
      }
      val keepLast = prev.tokenType != Whitespace ||
        (inKey && aheadAny(Dot, QuotedString, MultilineString)) ||
        (!inKey && aheadAny(LBrace, LBracket, QuotedString, MultilineString, Splice))
      val end = if (keepLast) idx else prev.idx
      val range = new HTokenRange(tokens, start, end)
      val value = range.iterator.map(_.text).mkString
      val syntax =
        if (end == start || end == start + 1 && tokens(start).tokenType == Whitespace) HStringSyntax.Whitespace
        else HStringSyntax.Unquoted
      HString(value)(syntax, range)
    } else {
      fail()
    }
  }

  def parseSubst(): HSubst = {
    val start = skipWs()
    ensure(Splice)
    ensure(LBrace)
    val optional = pass(Qmark)
    val path = parsePath()
    ensure(RBrace)
    HSubst(path)(optional, rangeFrom(start))
  }
}
