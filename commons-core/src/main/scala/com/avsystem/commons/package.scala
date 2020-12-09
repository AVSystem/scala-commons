package com.avsystem

import com.avsystem.commons.collection.CollectionAliases
import com.avsystem.commons.jiop.JavaInterop
import com.avsystem.commons.jsiop.JsInterop
import com.avsystem.commons.misc.MiscAliases

package object commons
  extends SharedExtensions with CommonAliases with MiscAliases with CollectionAliases with JavaInterop with JsInterop
