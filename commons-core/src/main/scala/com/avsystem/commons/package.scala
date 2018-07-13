package com.avsystem

import com.avsystem.commons.collection.CollectionAliases
import com.avsystem.commons.jiop.JavaInterop
import com.avsystem.commons.jsiop.JsInterop

package object commons
  extends SharedExtensions with CommonAliases with CollectionAliases with JavaInterop with JsInterop
