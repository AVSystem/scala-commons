package com.avsystem.commons
package hocon

import com.avsystem.commons.testutil.FileBasedSuite

class HLexerTest extends FileBasedSuite("/lexer") {
  test("simple") {
    testFile("hocon.txt") { contents =>
      val source = SourceFile(contents, "hocon.txt")
      val lexer = new HLexer(source)
      val tokens = lexer.tokenize()
      tokens.iterator
        .map { t =>
          s"<${t.pos.startLine + 1}:${t.pos.startColumn + 1}:$t:${t.pos.endLine + 1}:${t.pos.endColumn + 1}>"
        }
        .mkString("\n")
    }
  }
}
