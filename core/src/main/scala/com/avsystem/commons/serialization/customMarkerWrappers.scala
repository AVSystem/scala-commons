package com.avsystem.commons
package serialization

/**
  * [[Input]] implementation that adds additional markers [[CustomEventMarker]] to the provided [[Input]] instance
  */
final class CustomMarkersInputWrapper(
  override protected val wrapped: Input,
  markers: Set[CustomEventMarker[_]],
) extends InputWrapper {

  override def readList(): ListInput =
    new CustomMarkersInputWrapper.AdjustedListInput(super.readList(), markers)

  override def readObject(): ObjectInput =
    new CustomMarkersInputWrapper.AdjustedObjectInput(super.readObject(), markers)

  override def customEvent[T](marker: CustomEventMarker[T], value: T): Boolean =
    marker match {
      case marker if markers(marker) => true
      case _ => super.customEvent(marker, value)
    }
}
object CustomMarkersInputWrapper {
  def apply(input: Input, markers: CustomEventMarker[_]*): CustomMarkersInputWrapper =
    new CustomMarkersInputWrapper(input, markers.toSet)

  private final class AdjustedListInput(
    override protected val wrapped: ListInput,
    markers: Set[CustomEventMarker[_]],
  ) extends ListInputWrapper {
    override def nextElement(): Input = new CustomMarkersInputWrapper(super.nextElement(), markers)
    override def customEvent[T](marker: CustomEventMarker[T], value: T): Boolean =
      marker match {
        case marker if markers(marker) => true
        case _ => super.customEvent(marker, value)
      }
  }

  private final class AdjustedFieldInput(
    override protected val wrapped: FieldInput,
    markers: Set[CustomEventMarker[_]],
  ) extends FieldInputWrapper {

    override def readList(): ListInput = new AdjustedListInput(super.readList(), markers)
    override def readObject(): ObjectInput = new AdjustedObjectInput(super.readObject(), markers)

    override def customEvent[T](marker: CustomEventMarker[T], value: T): Boolean =
      marker match {
        case marker if markers(marker) => true
        case _ => super.customEvent(marker, value)
      }
  }

  private final class AdjustedObjectInput(
    override protected val wrapped: ObjectInput,
    markers: Set[CustomEventMarker[_]],
  ) extends ObjectInputWrapper {

    override def nextField(): FieldInput = new AdjustedFieldInput(super.nextField(), markers)

    override def peekField(name: String): Opt[FieldInput] =
      super.peekField(name).map(new AdjustedFieldInput(_, markers))

    override def customEvent[T](marker: CustomEventMarker[T], value: T): Boolean =
      marker match {
        case marker if markers(marker) => true
        case _ => super.customEvent(marker, value)
      }
  }
}

/**
  * [[Output]] implementation that adds additional markers [[CustomEventMarker]] to the provided [[Output]] instance
  */
final class CustomMarkersOutputWrapper(
  override protected val wrapped: Output,
  markers: Set[CustomEventMarker[_]],
) extends OutputWrapper {

  override def writeSimple(): SimpleOutput =
    new CustomMarkersOutputWrapper.AdjustedSimpleOutput(super.writeSimple(), markers)

  override def writeList(): ListOutput =
    new CustomMarkersOutputWrapper.AdjustedListOutput(super.writeList(), markers)

  override def writeObject(): ObjectOutput =
    new CustomMarkersOutputWrapper.AdjustedObjectOutput(super.writeObject(), markers)

  override def customEvent[T](marker: CustomEventMarker[T], value: T): Boolean =
    marker match {
      case marker if markers(marker) => true
      case _ => super.customEvent(marker, value)
    }
}

object CustomMarkersOutputWrapper {
  def apply(output: Output, markers: CustomEventMarker[_]*): CustomMarkersOutputWrapper =
    new CustomMarkersOutputWrapper(output, markers.toSet)

  private final class AdjustedSimpleOutput(
    override protected val wrapped: SimpleOutput,
    markers: Set[CustomEventMarker[_]],
  ) extends SimpleOutputWrapper {
    override def customEvent[T](marker: CustomEventMarker[T], value: T): Boolean =
      marker match {
        case marker if markers(marker) => true
        case _ => super.customEvent(marker, value)
      }
  }

  private final class AdjustedListOutput(
    override protected val wrapped: ListOutput,
    markers: Set[CustomEventMarker[_]],
  ) extends ListOutputWrapper {
    override def writeElement(): Output =
      new CustomMarkersOutputWrapper(super.writeElement(), markers)

    override def customEvent[T](marker: CustomEventMarker[T], value: T): Boolean =
      marker match {
        case marker if markers(marker) => true
        case _ => super.customEvent(marker, value)
      }
  }

  private final class AdjustedObjectOutput(
    override protected val wrapped: ObjectOutput,
    markers: Set[CustomEventMarker[_]],
  ) extends ObjectOutputWrapper {
    override def writeField(key: String): Output =
      new CustomMarkersOutputWrapper(super.writeField(key), markers)

    override def customEvent[T](marker: CustomEventMarker[T], value: T): Boolean =
      marker match {
        case marker if markers(marker) => true
        case _ => super.customEvent(marker, value)
      }
  }
}
