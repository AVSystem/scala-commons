package com.avsystem.commons
package hocon

import com.avsystem.commons.testutil.FileBasedSuite

class HParserTest extends FileBasedSuite("/parser") {
  test("simple") {
    testFile("hocon.txt") { contents =>
      val source = SourceFile(contents, "hocon.txt")
      val parser = new HParser(new HLexer(source).tokenize())
      HTree.repr(parser.parseSource())
    }
  }
}
