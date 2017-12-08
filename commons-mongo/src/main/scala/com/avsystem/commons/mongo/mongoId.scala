package com.avsystem.commons
package mongo

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.{name, outOfOrder}

@name("_id") @outOfOrder
class mongoId extends AnnotationAggregate
