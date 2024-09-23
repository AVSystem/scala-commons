package com.avsystem.commons
package serialization

trait AcceptsAdditionalCustomMarkers extends AcceptsCustomEvents {

  protected def markers: Set[CustomEventMarker[_]]

  override def customEvent[T](marker: CustomEventMarker[T], event: T): Boolean =
    marker match {
      case marker if markers(marker) => true
      case _ => super.customEvent(marker, event)
    }
}

/**
  * [[Input]] implementation that adds additional markers [[CustomEventMarker]] to the provided [[Input]] instance
  */
final class CustomMarkersInputWrapper(
  override protected val wrapped: Input,
  override protected val markers: Set[CustomEventMarker[_]],
) extends InputWrapper with AcceptsAdditionalCustomMarkers {

  override def readList(): ListInput =
    new CustomMarkersInputWrapper.AdjustedListInput(super.readList(), markers)

  override def readObject(): ObjectInput =
    new CustomMarkersInputWrapper.AdjustedObjectInput(super.readObject(), markers)
}
object CustomMarkersInputWrapper {
  def apply(input: Input, markers: CustomEventMarker[_]*): CustomMarkersInputWrapper =
    new CustomMarkersInputWrapper(input, markers.toSet)

  private final class AdjustedListInput(
    override protected val wrapped: ListInput,
    override protected val markers: Set[CustomEventMarker[_]],
  ) extends ListInputWrapper with AcceptsAdditionalCustomMarkers {
    override def nextElement(): Input = new CustomMarkersInputWrapper(super.nextElement(), markers)
  }

  private final class AdjustedFieldInput(
    override protected val wrapped: FieldInput,
    override protected val markers: Set[CustomEventMarker[_]],
  ) extends FieldInputWrapper with AcceptsAdditionalCustomMarkers {

    override def readList(): ListInput = new AdjustedListInput(super.readList(), markers)
    override def readObject(): ObjectInput = new AdjustedObjectInput(super.readObject(), markers)
  }

  private final class AdjustedObjectInput(
    override protected val wrapped: ObjectInput,
    override protected val markers: Set[CustomEventMarker[_]],
  ) extends ObjectInputWrapper with AcceptsAdditionalCustomMarkers {

    override def nextField(): FieldInput = new AdjustedFieldInput(super.nextField(), markers)
    override def peekField(name: String): Opt[FieldInput] =
      super.peekField(name).map(new AdjustedFieldInput(_, markers))
  }
}

/**
  * [[Output]] implementation that adds additional markers [[CustomEventMarker]] to the provided [[Output]] instance
  */
final class CustomMarkersOutputWrapper(
  override protected val wrapped: Output,
  override protected val markers: Set[CustomEventMarker[_]],
) extends OutputWrapper with AcceptsAdditionalCustomMarkers {

  override def writeSimple(): SimpleOutput =
    new CustomMarkersOutputWrapper.AdjustedSimpleOutput(super.writeSimple(), markers)

  override def writeList(): ListOutput =
    new CustomMarkersOutputWrapper.AdjustedListOutput(super.writeList(), markers)

  override def writeObject(): ObjectOutput =
    new CustomMarkersOutputWrapper.AdjustedObjectOutput(super.writeObject(), markers)
}

object CustomMarkersOutputWrapper {
  def apply(output: Output, markers: CustomEventMarker[_]*): CustomMarkersOutputWrapper =
    new CustomMarkersOutputWrapper(output, markers.toSet)

  private final class AdjustedSimpleOutput(
    override protected val wrapped: SimpleOutput,
    override protected val markers: Set[CustomEventMarker[_]],
  ) extends SimpleOutputWrapper with AcceptsAdditionalCustomMarkers

  private final class AdjustedListOutput(
    override protected val wrapped: ListOutput,
    override protected val markers: Set[CustomEventMarker[_]],
  ) extends ListOutputWrapper with AcceptsAdditionalCustomMarkers {

    override def writeElement(): Output =
      new CustomMarkersOutputWrapper(super.writeElement(), markers)
  }

  private final class AdjustedObjectOutput(
    override protected val wrapped: ObjectOutput,
    override protected val markers: Set[CustomEventMarker[_]],
  ) extends ObjectOutputWrapper with AcceptsAdditionalCustomMarkers {

    override def writeField(key: String): Output =
      new CustomMarkersOutputWrapper(super.writeField(key), markers)
  }
}
