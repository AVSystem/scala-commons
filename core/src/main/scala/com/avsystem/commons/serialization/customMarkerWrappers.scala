package com.avsystem.commons
package serialization

trait AcceptsAdditionalCustomMarkers extends AcceptsCustomEvents {

  protected def markers: Set[CustomEventMarker[?]]

  override def customEvent[T](marker: CustomEventMarker[T], event: T): Boolean =
    markers(marker) || super.customEvent(marker, event)
}

/**
  * [[Input]] implementation that adds additional markers [[CustomEventMarker]] to the provided [[Input]] instance
  */
final class CustomMarkersInputWrapper private(
  override protected val wrapped: Input,
  override protected val markers: Set[CustomEventMarker[?]],
) extends InputWrapper with AcceptsAdditionalCustomMarkers {

  override def readList(): ListInput =
    new CustomMarkersInputWrapper.AdjustedListInput(super.readList(), markers)

  override def readObject(): ObjectInput =
    new CustomMarkersInputWrapper.AdjustedObjectInput(super.readObject(), markers)
}
object CustomMarkersInputWrapper {
  def apply(input: Input, markers: CustomEventMarker[?]*): CustomMarkersInputWrapper =
    CustomMarkersInputWrapper(input, markers.toSet)

  def apply(input: Input, markers: Set[CustomEventMarker[?]]): CustomMarkersInputWrapper =
    new CustomMarkersInputWrapper(input, markers)

  private final class AdjustedListInput(
    override protected val wrapped: ListInput,
    override protected val markers: Set[CustomEventMarker[?]],
  ) extends ListInputWrapper with AcceptsAdditionalCustomMarkers {
    override def nextElement(): Input = new CustomMarkersInputWrapper(super.nextElement(), markers)
  }

  private final class AdjustedFieldInput(
    override protected val wrapped: FieldInput,
    override protected val markers: Set[CustomEventMarker[?]],
  ) extends FieldInputWrapper with AcceptsAdditionalCustomMarkers {

    override def readList(): ListInput = new AdjustedListInput(super.readList(), markers)
    override def readObject(): ObjectInput = new AdjustedObjectInput(super.readObject(), markers)
  }

  private final class AdjustedObjectInput(
    override protected val wrapped: ObjectInput,
    override protected val markers: Set[CustomEventMarker[?]],
  ) extends ObjectInputWrapper with AcceptsAdditionalCustomMarkers {

    override def nextField(): FieldInput = new AdjustedFieldInput(super.nextField(), markers)
    override def peekField(name: String): Opt[FieldInput] =
      super.peekField(name).map(new AdjustedFieldInput(_, markers))
  }
}

/**
  * [[Output]] implementation that adds additional markers [[CustomEventMarker]] to the provided [[Output]] instance
  */
final class CustomMarkersOutputWrapper private(
  override protected val wrapped: Output,
  override protected val markers: Set[CustomEventMarker[?]],
) extends OutputWrapper with AcceptsAdditionalCustomMarkers {

  override def writeSimple(): SimpleOutput =
    new CustomMarkersOutputWrapper.AdjustedSimpleOutput(super.writeSimple(), markers)

  override def writeList(): ListOutput =
    new CustomMarkersOutputWrapper.AdjustedListOutput(super.writeList(), markers)

  override def writeObject(): ObjectOutput =
    new CustomMarkersOutputWrapper.AdjustedObjectOutput(super.writeObject(), markers)
}

object CustomMarkersOutputWrapper {
  def apply(output: Output, markers: CustomEventMarker[?]*): CustomMarkersOutputWrapper =
    CustomMarkersOutputWrapper(output, markers.toSet)

  def apply(output: Output, markers: Set[CustomEventMarker[?]]): CustomMarkersOutputWrapper =
    new CustomMarkersOutputWrapper(output, markers)

  private final class AdjustedSimpleOutput(
    override protected val wrapped: SimpleOutput,
    override protected val markers: Set[CustomEventMarker[?]],
  ) extends SimpleOutputWrapper with AcceptsAdditionalCustomMarkers

  private final class AdjustedListOutput(
    override protected val wrapped: ListOutput,
    override protected val markers: Set[CustomEventMarker[?]],
  ) extends ListOutputWrapper with AcceptsAdditionalCustomMarkers {

    override def writeElement(): Output =
      new CustomMarkersOutputWrapper(super.writeElement(), markers)
  }

  private final class AdjustedObjectOutput(
    override protected val wrapped: ObjectOutput,
    override protected val markers: Set[CustomEventMarker[?]],
  ) extends ObjectOutputWrapper with AcceptsAdditionalCustomMarkers {

    override def writeField(key: String): Output =
      new CustomMarkersOutputWrapper(super.writeField(key), markers)
  }
}
