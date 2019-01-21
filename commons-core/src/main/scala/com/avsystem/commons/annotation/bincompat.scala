package com.avsystem.commons
package annotation

/**
  * Marks symbols which exist only for binary compatibility with previous versions.
  * These symbols should never be used directly. This is checked by `commons-analyzer` plugin.
  */
class bincompat extends Annotation
