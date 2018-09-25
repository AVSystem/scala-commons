# Annotation processing

Various macro engines used in AVSystem Commons library inspect annotations on classes, methods,
parameters and types. For example, [`GenCodec`](GenCodec.md) materialization inspects annotation
that may adjust how the codec is made.

Because of heavy use of annotations, some common rules, conventions and utilities have been created
for annotation processing in all macro engines implemented by AVSystem Commons. They allow
the programmer to reduce boilerplate by reusing and grouping annotations.

Mechanisms described below are all implemented in `MacroCommons` base trait used in macro implementations.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Annotation processing](#annotation-processing)
    - [Annotation inheritance](#annotation-inheritance)
    - [Extending existing annotations](#extending-existing-annotations)
    - [Aggregating annotations](#aggregating-annotations)
    - [Annotation order](#annotation-order)
    - [`@defaultsToName`](#defaultstoname)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

### Annotation inheritance

When a `MacroCommons`-based macro looks for annotations, they are automatically _inherited_. What that means
exactly depends on the target symbol of an annotation:

* Annotations applied on a class or trait are automatically inherited by all their subtraits, subclasses
  and objects. The only exception is when an annotation extending `NotInheritedFromSealedTypes` is applied
  on a `sealed` trait or class.
* Annotations applied on a member (`def`, `val`, `var`) are automatically inherited by all members that
  implement or override it.
* Annotations applied on method parameters (or type parameters) are automatically inherited by corresponding
  parameters in all implementing and overriding methods.
* Annotations applied on `val` or `var` definitions which correspond to constructor parameters are inherited
  by these constructor parameters themselves.

### Extending existing annotations

You may create subclasses of existing annotations used by macro engines. For example, we may create a subclass
of `@name` annotation used by `GenCodec` materialization:

```scala
import com.avsystem.commons.serialization._

class customName(override val name: String) extends name(name)
```

Now, when the macro looks for `@name` annotation, it will also find `@customName` because of their
subtyping relation. However, in order for the macro to be able to properly _statically_ (in compile time)
extract the `name` constructor parameter, it must be declared in exactly like in the example above, i.e.
it must be an `override val` constructor parameter which overrides the original one.

Note that for such statically inspected annotation parameters you cannot simply pass them as super constructor
parameters, e.g. the following will **NOT** work:

```scala
class id extends name("id") // doesn't work
```

This is because super constructor parameters are not available for macros in compile time.
The same effect can however be achieved using [annotation aggregates](#aggregating-annotations)

### Aggregating annotations

It is possible to create your own annotations which group - or aggregate - multiple other annotations.
This way you can reduce boilerplate associated with annotations.

In order to do that, you must create an annotation class that extends `AnnotationAggregate` and
redefine its dummy `Implied` type member. You can then put your aggregated annotations on that type
member, e.g.

```scala
import com.avsystem.commons.annotation._
import com.avsystem.commons.serialization._

class id extends AnnotationAggregate {
  @name("id") type Implied
}
```

Now, when something is annotated as `@id`, macro engines will effectively "explode" this annotation
into all annotations applied on the `Implied` member. This is done recursively which means that
the `Implied` member itself may be annotated with more aggregating annotations.

Having to redefine the dummy `Implied` type member may seem strange. It would seem be more natural to
put aggregated annotation on the aggregate itself, e.g. `@name("id") class id extends AnnotationAggregate`
(this doesn't work). However, having aggregate annotation on a type member lets us refer to parameters
of the aggregate itself, e.g.

```scala
import com.avsystem.commons.annotation._
import com.avsystem.commons.serialization._

class customName(name: String) extends AnnotationAggregate {
  @name(name) type Implied
}
```

Now, when "exploding" the aggregate, the macro engine will _statically_ replace references to
constructor parameters of the aggregate inside `Implied` annotations. For example, when something
is annotated as `@customName("id")` the macro will effectively see `@name("id")`.

### Annotation order

Scala allows annotating symbols with the same annotation multiple times. Also, annotation may be
repeated because of [inheritance](#annotation-inheritance) - the same annotation may be applied on
a symbol directly and inherited. In such situations, the order of annotations is well defined:

* When two annotations are applied _directly_ on the same symbol, the first one takes precedence, e.g.
  `@name("A") @name("B") class C` - `@name("A")` has precedence over `@name("B")`
* Annotations applied directly have precedence over inherited annotations.
* Among inherited annotations, precedence is determined based on linearization order and therefore
  is analogous to how class/trait members override themselves.

With respect to the order defined above, macro engines may sometimes take only the _first_ annotation
of given type or sometimes inspect _all_ of them, in that order. This depends on particular annotation
and how it's understood by particular macro engine.

### `@defaultsToName`

`@defaultsToName` is a meta-annotation that you may put on a `String` typed constructor parameter
of an annotation. This parameter may then take a dummy default value (e.g. `null`) and macro engines
will replace that default value with annotated symbol's original (source) name. For example:

```scala
class awesome(@defaultsToName rawName: String = null) extends StaticAnnotation
```

Now, annotating a class with `@awesome` without giving the `rawName` argument explicitly:

```scala
@awesome class Klass
```

is equivalent to annotating it as `@awesome("Klass")`.

This of course works for all kinds of symbols that can be annotated, not only classes.
