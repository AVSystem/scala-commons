package com.avsystem.commons
package annotation

/**
  * Marks symbols which exist only for binary compatibility with previous versions.
  * These symbols should never be used directly. This is checked by `commons-analyzer` plugin.
  * Additionally, it's recommended to make these symbols package private so that they cannot be used
  * directly from Scala code but remain public in bytecode.
  */
class bincompat extends Annotation
