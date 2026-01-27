package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.InputMetadata

enum JsonType {
  case list, `object`, number, string, boolean, `null`
}

object JsonType extends InputMetadata[JsonType]
