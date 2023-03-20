package com.avsystem.commons
package annotation

/**
  * When applied on a definition (`class`, `object`, `def`, `val`, etc.) or expression, will cause the
  * AVSystem static analyzer to print compilation error with AST of annotated code fragment.
  * This is useful primarily for debugging macro expansions.
  */
class showAst extends StaticAnnotation
