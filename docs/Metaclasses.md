# Metaclasses

## Overview

### What is it

_Metaclasses_ is a macro-powered library with a set of annotations that lets you define and derive classes that collect metadata about other classes.
It can be thought of as a reflection library for Scala and serves a similar purpose as various typeclass derivation libraries like 
[shapeless](https://github.com/milessabin/shapeless) or [magnolia](https://github.com/softwaremill/magnolia).

_Metaclasses_ let you capture the structure of:
* case classes and sealed hierarchies
* "RPC" traits, i.e. traits whose public abstract methods are of interest
* full public API of practically any Scala type

### Supported metadata

The metadata that can be collected with _metaclasses_ includes:
* for case classes: metadata about its parameters
* for sealed hierarchies: metadata about its every case class/object and intermediate sealed traits
* for "RPC" traits: metadata about its abstract methods
* for arbitrary types: metadata about all its public members
* for every method: metadata about its parameters, type parameters and result type (i.e. practically full signature)
* for every type, method result type or parameter type: selected typeclass instances (implicits)
* for every symbol (class, trait, object, method, parameter): information about its name, source position, flags (e.g. `final`, `abstract`)
* for every symbol: its annotations
* for every case class parameter: its default value

The most important capability of _metaclasses_ is an ability to precisely control which pieces of metadata listed above are actually collected and
reified into runtime, and for which types. Note how this is different from "traditional" runtime reflection frameworks that unconditionally capture _all_
available information for _all_ types and still may be missing important information known only by the compiler (e.g. implicits).

### Tagging

_Metaclasses_ also support _tagging_, a mechanism that lets you group symbols (e.g. methods, case class parameters) into categories that are 
treated differently, i.e. a different metadata may be reified for a case class parameter tagged with some specific annotation. For example: 
[Udash REST](https://guide.udash.io/rest) defines several kinds of parameters in REST methods, e.g. `@Path`, `@Query`, `@Header`, `@Body`, etc.
Each of them potentially requires a different implicit for serialization. The _metaclasses_ and RPC framework can distinguish between them already
in compile time (inside macros that power these libraries).

## Quickstart

In this quickstart example, we will design a _metaclass_ that captures a structure of an arbitrary case class. Captured information contains:

* the name of the case class
* the name of every parameter
* `TypeString` typeclass instance for every parameter type: it captures the textual representation of a Scala type, as written in source code

```scala
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.TypeString

final class CaseClassMeta[T](
  @reifyName val name: String,
  @multi @adtParamMetadata val params: List[CaseClassMeta.ParamMeta[_]],
)
object CaseClassMeta extends AdtMetadataCompanion[CaseClassMeta] {
  final class ParamMeta[T](
    @reifyName val name: String,
    @infer val typeString: TypeString[T],
  ) extends TypedMetadata[T]
}
```

Now we will need some boilerplate so that deriving this metadata for case classes is as concise as possible.
Please ignore the details for now.

```scala
abstract class DataCompanion[T](implicit instances: MacroInstances[Unit, () => CaseClassMeta[T]]) {
  implicit lazy val caseClassMeta: CaseClassMeta[T] = instances((), this).apply()
}
```

Finally, we can define our case classes with this metadata derived:

```scala
case class SomeData(
  text: String,
  number: Int,
)
object SomeData extends DataCompanion[SomeData]
```
