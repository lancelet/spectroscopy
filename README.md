spectroscopy
============

[![Build Status](https://travis-ci.com/CommBank/spectroscopy.svg?token=kTaqFzyDy4pVMsrWYLYn)](https://travis-ci.com/CommBank/spectroscopy)

The `spectroscopy` library extends the Monocle library of optics with an
optic-like type which we call a Scope. A Scope is similar to an Optional (also
known as an affine Traversal), but with a stronger `set` method called `put`.
"Scope" is short for Spectroscope, so-named because Spectroscopes can be formed
by composing a Lens with a Prism.

A Scope is optic-*like* because it defines getter and setter methods which
follow laws similar to those of many optics, but unlike standard optics Scopes
have some restrictions on composability.

Scopes vs. Optionals
--------------------

Composing a Lens with a Prism in the usual manner results in an Optional. An
`Optional[S, A]` must satisfy the following laws for every `s: S`, `a: A`, and
`b: B`:

1.  You get back what you put in (if and only if there was already a value in there):

    ```scala
    getOption(set(a)(s)) <==> getOption(s).map(_ => a)
    ```
2.  Putting back what you got doesn't change anything:

    ```scala
    getOrModify(s).fold(identity, set(_)(s)) <==> s
    ```
3.  Setting twice is the same as setting once:

    ```scala
    set(a)(set(b)(s)) <==> set(a)(s)
    ```

A Scope may be composed from a Lens and a Prism. A `Scope[S, A]` must satisfy
the following laws for every `s: S`, `a: A`, `b: B`:

1.  You get back what you put in (*always!*)

    ```scala
    getOption(put(a)(s)) <==> Some(a)
    ```
2.  Putting back what you got doesn't change anything:

    ```scala
    getOrModify(s).fold(identity, put(_)(s)) <==> s
    ```
3.  Setting twice is the same as setting once:

    ```scala
    put(a)(put(b)(s)) <==> put(a)(s)
    ```

The difference between the `set` method of an Optional and the `put` method of
a Scope is in the first law. A `set` followed by a `getOption` will only return
a value if `getOption` initially returned a value, i.e. `set` only replaces
existing values, whereas a `put` followed by a `getOption` will *always* return
a value, i.e. `put` will add a value if it doesn't already exist.

Examples
--------

If Lenses are for dealing with product types and Prisms are for dealing with
sum types, then Scopes are for dealing with product types containing sum types.

```scala
val eitherMap = Map(
  "foo" -> Left("Apple"),
  "bar" -> Right(42)
)
```

We can compose Lenses and Prisms into Optionals in order to access values
inside `eitherMap`:

```scala
import monocle.std.either._
import monocle.std.map._
import monocle.std.option._

val eitherAtMap    = atMap[String, Either[String, Int]]

val _SomeLeftAtFoo = eitherAtMap.at("foo") composePrism some composePrism stdLeft
val _SomeLeftAtBar = eitherAtMap.at("bar") composePrism some composePrism stdLeft
val _SomeLeftAtBaz = eitherAtMap.at("baz") composePrism some composePrism stdLeft
val _NoneAtFoo     = eitherAtMap.at("foo") composePrism none
val _NoneAtBaz     = eitherAtMap.at("baz") composePrism none
```

The Optionals can get values from the `eitherMap`:

```scala
scala> _SomeLeftAtFoo.getOption(eitherMap)
res0: Option[String] = Some(Apple)

scala> _SomeLeftAtBar.getOption(eitherMap)
res1: Option[String] = None

scala> _SomeLeftAtBaz.getOption(eitherMap)
res2: Option[String] = None

scala> _NoneAtFoo.getOption(eitherMap)
res3: Option[Unit] = None

scala> _NoneAtBaz.getOption(eitherMap)
res4: Option[Unit] = Some(())
```

The Optionals can also `set` values inside the `eitherMap`:

```scala
scala> _SomeLeftAtFoo.set("Banana")(eitherMap)
res5: Map[String,Either[String,Int]] = Map(foo -> Left(Banana), bar -> Right(42))

scala> _SomeLeftAtBar.set("Banana")(eitherMap)
res6: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))

scala> _SomeLeftAtBaz.set("Banana")(eitherMap)
res7: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))

scala> _NoneAtFoo.set(())(eitherMap)
res8: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))

scala> _NoneAtBaz.set(())(eitherMap)
res9: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))
```

However the Optionals will only successfully set a value at a key of the
`eitherMap` if the existing value matches the Prism composed in the Optional.

For our use case the desired behaviour was to replace the value at the key of
the `eitherMap` with the result of calling `reverseGet` using the specified
Prism. We encapsulated this behaviour into a Scope. Composing Scopes from
Lenses and Prisms is similar to composing Optionals:

```scala
import au.com.cba.omnia.spectroscopy._

val _SomeLeftAtFoo_ = eitherAtMap.at("foo").asScope composePrism some composePrism stdLeft
val _SomeLeftAtBar_ = eitherAtMap.at("bar").asScope composePrism some composePrism stdLeft
val _SomeLeftAtBaz_ = eitherAtMap.at("baz").asScope composePrism some composePrism stdLeft
val _NoneAtFoo_     = eitherAtMap.at("foo").asScope composePrism none
val _NoneAtBaz_     = eitherAtMap.at("baz").asScope composePrism none
```

The `get` method behaves identically to the `get` method from Optional:

```scala
scala> _SomeLeftAtFoo_.getOption(eitherMap)
res10: Option[String] = Some(Apple)

scala> _SomeLeftAtBar_.getOption(eitherMap)
res11: Option[String] = None

scala> _SomeLeftAtBaz_.getOption(eitherMap)
res12: Option[String] = None

scala> _NoneAtFoo_.getOption(eitherMap)
res13: Option[Unit] = None

scala> _NoneAtBaz_.getOption(eitherMap)
res14: Option[Unit] = Some(())
```

However compared to the `set` method of the Optional, the `put` method always
succeeds:

```scala
scala> _SomeLeftAtFoo_.put("Banana")(eitherMap)
res15: Map[String,Either[String,Int]] = Map(foo -> Left(Banana), bar -> Right(42))

scala> _SomeLeftAtBar_.put("Banana")(eitherMap)
res16: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Left(Banana))

scala> _SomeLeftAtBaz_.put("Banana")(eitherMap)
res17: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42), baz -> Left(Banana))

scala> _NoneAtFoo_.put(())(eitherMap)
res18: Map[String,Either[String,Int]] = Map(bar -> Right(42))

scala> _NoneAtBaz_.put(())(eitherMap)
res19: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))
```

We can also call `set` on a Scope to get the same behaviour as an Optional:

```scala
scala> _SomeLeftAtFoo_.set("Banana")(eitherMap)
res20: Map[String,Either[String,Int]] = Map(foo -> Left(Banana), bar -> Right(42))

scala> _SomeLeftAtBar_.set("Banana")(eitherMap)
res21: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))

scala> _SomeLeftAtBaz_.set("Banana")(eitherMap)
res22: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))

scala> _NoneAtFoo_.set(())(eitherMap)
res23: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))

scala> _NoneAtBaz_.set(())(eitherMap)
res24: Map[String,Either[String,Int]] = Map(foo -> Left(Apple), bar -> Right(42))
```

Limitations
-----------

### Composability of Scopes

Scopes compose on the left with Lenses and on the right with Prisms to form new
Scopes. Unlike the standard optics, Scopes do not generally compose with other
Scopes to form new Scopes. This is due to the strengthened requirements of the
setter method compared to an Optional.

To show that Scopes do not generally compose, we provide an example of two
Scopes that cannot compose.

```scala
def _Left[A] = PScope[A, A, Nothing, Nothing](
  s => -\/(s)
)(
  b => s => ???
)

def _Right[B] = PScope[Nothing, Nothing, B, B](
  s => ???
)(
  b => s => ???
)
```

For all types `A` and `B` we note that `_Left[A]` and `_Right[B]` satisfy the
Scope laws. In the case of `_Left[A]`, Scope laws (1) and (3) are trivially
satisfied because they are universally quantified over the values inhabited by
the type `Nothing`, and Scope law (2) is satisfied by always returning
`-\/(s)`.  In the case of `_Right[B]`, all of the Scope laws are universally
quantified over the values in `Nothing`, and so all of the Scope laws are
trivially satisfied.

We will show that any well-typed `composeScope` method will result in an object
that doesn't satisfy the Scope laws when applied to specific instances of
`_Left[A]` and `_Right[B]`. Let `composeScope` be a method defined on the
abstract class `PScope[S, T, A, B]` with the type signature
`composeScope[U, V](other: PScope[A, B, U, V]): PScope[S, T, U, V]`,
let `A` and `B` be inhabited types, and
let `val _LeftRight = _Left[A] composeScope _Right[B]`.
Then `_LeftRight.getOption` has the type signature
`_LeftRight.getOption(s: A): Option[B]`. As the method `composeScope`
is defined on the abstract class `PScope[S, T, A, B]`, it cannot itself create
a value of type `B`, it can only get a value of type `B` by
calling methods from `_Left[A]` or `_Right[B]`. The methods of
`_Left[A]` do not return a `B` due to their type signatures. The methods
of `_Right[B]` cannot be called because their type signatures require
arguments of type Nothing, hence these methods do not return a `B`.
So for every `s: A` we have `_LeftRight.getOption(s) == None`, and thus for
every `a: A` we have `getOption(put(a)(s)) == None != Some(a)`. Therefore
`_LeftRight` doesn't satisfy the Scope laws.
