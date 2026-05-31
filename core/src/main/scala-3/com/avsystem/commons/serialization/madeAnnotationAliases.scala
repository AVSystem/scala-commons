package com.avsystem.commons
package serialization

/**
 * Source-compatibility re-exports for annotations that moved into the `made` library.
 * Downstream code that references `com.avsystem.commons.serialization.{generated, name,
 * optionalParam, transparent, whenAbsent}` continues to compile against the made-provided
 * annotations.
 */
export made.annotation.generated
export made.annotation.name
export made.annotation.optionalParam
export made.annotation.transparent
export made.annotation.whenAbsent
export made.TransparentWrapping
