/*
 * Copyright 2015 Commonwealth Bank of Australia
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package au.com.cba.omnia.spectroscopy

import monocle.{
  Fold,
  Getter,
  Iso,
  PIso,
  PLens,
  POptional,
  PPrism,
  PSetter,
  PTraversal
}
import scalaz.{\/, -\/, \/-, Applicative}

/**
 * An [[EPScope]] can be seen as a pair of functions:
 *  - `getOrModifyWithError: S => (E, T) \/ A`
 *  - `put: B => S => T`
 *
 * [[EScope]] stands for Error-reporting Scope.
 *
 * An [[EPScope]] could also be defined as a PScope where the getter method
 * returns an error message if the PScope isn't matching.
 * 
 * Typically a [[EPScope]] or [[EScope]] corresponds to the composition of a
 * PLens and an [[EPPrism]], thus encoding the relation between a Product of
 * CoProduct types (e.g. a map with case class values) and one of its elements.
 *
 * [[EScope]] is a type alias for [[EPScope]] where the type of target cannot be
 * modified:
 * {{{
 * type EScope[E, S, A] = EPScope[E, S, S, A, A]
 * }}}
 *
 * An [[EPScope]] is also a valid Fold, [[PScope]], POptional, PTraversal, and
 * PSetter.
 *
 * @see [[au.com.cba.omnia.spectroscopy.law.EScopeLaws]]
 *
 * @tparam E the error message returned by a [[EPScope]]
 * @tparam S the source of a [[EPScope]]
 * @tparam T the modified source of a [[EPScope]]
 * @tparam A the target of a [[EPScope]]
 * @tparam B the modified target of a [[EPScope]]
 *
 */
abstract class EPScope[E, S, T, A, B] extends Serializable { self =>
  /** Get the target of an [[EPScope]] or return an error message and the the
   *  original value and while allowing the type to change if it does not match
   *  */
  def getOrModifyWithError(s: S): (E, T) \/ A

  /** Put polymorphically the target of an [[EPScope]] with a value */
  def put(b: B): S => T

  /** Get the target of an [[EPScope]] or return the original value while
   *  allowing the type to change if it does not match */
  def getOrModify(s: S): T \/ A

  /** Get the target of an [[EPScope]] or an error message if there is no
   *  target */
  def getOrError(s: S): E \/ A

  /** Get the target of an [[EPScope]] or nothing if there is no target */
  def getOption(s: S): Option[A]

  /** Modify polymorphically the target of an [[EPScope]] with a function */
  def modify(f: A => B): S => T

  /** Modify polymorphically the target of an [[EPScope]] with an Applicative
   *  function */
  def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T]

  /** Modify polymorphically the target of an [[EPScope]] with a function.
   *  return an error message if the [[EPScope]] is not matching */
  @inline final def modifyOrError(f: A => B): S => E \/ T =
    s => getOrError(s).map(a => put(f(a))(s))

  /** 
   *  Modify polymorphically the target of an [[EPScope]] with a function.
   *  Return nothing if the [[EPScope]] is not matching
   */
  @inline final def modifyOption(f: A => B): S => Option[T] =
    modifyOrError(f)(_).toOption

  /** Set polymorphically the target of an [[EPScope]] using a function */
  @inline final def set(b: B): S => T =
    modify(_ => b)

  /** Set polymorphically the target of an [[EPScope]] with a value. 
   * return an error message if the [[EPScope]] is not matching */
  @inline final def setOrError(b: B): S => E \/ T =
    modifyOrError(_ => b)

  /**
   * Set polymorphically the target of an [[EPScope]] with a value.
   * Return nothing if the [[EPScope]] is not matching.
   */
  @inline final def setOption(b: B): S => Option[T] =
    modifyOption(_ => b)

  /** Check if an [[EPScope]] has a target */
  @inline final def isMatching(s: S): Boolean =
    getOption(s).isDefined

  /** Join two [[EPScope]] with the same target */
  @inline final def choice[E1, S1, T1](other: EPScope[E, S1, T1, A, B]): EPScope[E, S \/ S1, T \/ T1, A, B] =
    EPScope[E, S \/ S1, T \/ T1, A, B](
      _.fold(
        self.getOrModifyWithError(_) .leftMap(
          et => et.copy(_2 = -\/(et._2))
        ),
        other.getOrModifyWithError(_) .leftMap(
          et1 => et1.copy(_2 = \/-(et1._2))
        )
      )
    )(
      b => _.bimap(self.put(b), other.put(b))
    )

  @inline final def first[C]: EPScope[E, (S, C), (T, C), (A, C), (B, C)] =
    EPScope[E, (S, C), (T, C), (A, C), (B, C)]{
      case (s, c) => getOrModifyWithError(s).bimap(
        et => et.copy(_2 = et._2 -> c),
        _ -> c
      )
    }{
      case (b, c) => {
        case (s, _) => (put(b)(s), c)
      }
    }

  @inline final def second[C]: EPScope[E, (C, S), (C, T), (C, A), (C, B)] =
    EPScope[E, (C, S), (C, T), (C, A), (C, B)]{
      case (c, s) => getOrModifyWithError(s).bimap(
        et => et.copy(_2 = c -> et._2),
        c -> _
      )
    }{
      case (c, b) => {
        case (_, s) => (c, put(b)(s))
      }
    }

  @inline final def mapError[F](f: E => F): EPScope[F, S, T, A, B] =
    EPScope[F, S, T, A, B](
      getOrModifyWithError(_).leftMap(et =>
        et.copy(_1 = f(et._1))
      )
    )(put(_))

  /**************************************************************/
  /** Compose methods between an [[EPScope]] and another Optics */
  /**************************************************************/

  /** Compose an [[EPScope]] with a PIso */
  @inline final def composeIso[U, V](
    other: PIso[A, B, U, V]
  ): EPScope[Option[E], S, T, U, V] =
    self composeEPrismLeft other.asEPrism

  /** Compose an [[EPScope]] with an [[EPPrism]] */
  @inline final def composeEPrism[F, U, V](
    other: EPPrism[F, A, B, U, V]
  ): EPScope[E \/ F, S, T, U, V] =
    EPScope[E \/ F, S, T, U, V](
      s =>
        self
          .getOrModifyWithError(s)
          .leftMap(
            et => et.copy(_1 = -\/(et._1))
          )
          .flatMap(a => 
            other
              .getOrModifyWithError(a)
              .bimap(
                eb => (
                  \/-(eb._1),
                  self.set(eb._2)(s)
                ),
                identity
              )
          )
    )(
      u => s => put(other.reverseGet(u))(s)
    )

  @inline final def composeEPrismLeft[F, U, V](
    other: EPPrism[F, A, B, U, V]
  ): EPScope[Option[E], S, T, U, V] =
    (self composeEPrism other).leftError

  @inline final def composeEPrismRight[F, U, V](
    other: EPPrism[F, A, B, U, V]
  ): EPScope[Option[F], S, T, U, V] =
    (self composeEPrism other).rightError

  /** Compose an [[EPScope]] with a PPrism */
  @inline final def composePrism[U, V](
    other: PPrism[A, B, U, V]
  ): EPScope[Option[E], S, T, U, V] =
    self composeEPrismLeft other.asEPrism

  /** Compose an [[EPScope]] with a POptional */
  @inline final def composeOptional[U, V](
    other: POptional[A, B, U, V]
  ): POptional[S, T, U, V] =
    asOptional composeOptional other

  /** Compose an [[EPScope]] with a Fold */
  @inline final def composeFold[C](other: Fold[A, C]): Fold[S, C] =
    asFold composeFold other

  /** Compose an [[EPScope]] with a Getter */
  @inline final def composeGetter[C](other: Getter[A, C]): Fold[S, C] =
    asFold composeGetter other

  /** Compose an [[EPScope]] with a PSetter */
  @inline final def composeSetter[U, V](
    other: PSetter[A, B, U, V]
  ): PSetter[S, T, U, V] =
    asSetter composeSetter other

  /** Compose an [[EPScope]] with a PTraversal */
  @inline final def composeTraversal[U, V](
    other: PTraversal[A, B, U, V]
  ): PTraversal[S, T, U, V] =
    asTraversal composeTraversal other

  /** Compose an [[EPScope]] with a PLens */
  @inline final def composeLens[U, V](
    other: PLens[A, B, U, V]
  ): POptional[S, T, U, V] =
    asOptional composeOptional other.asOptional

  /********************************************************************/
  /** Transformation methods to view an [[EPScope]] as another Optics */
  /********************************************************************/

  /** View an [[EPScope]] as a [[PScope]] */
  @inline final def asScope: PScope[S, T, A, B] =
    PScope[S, T, A, B](getOrModify(_))(put(_))

  /** View an [[EPScope]] as a POptional */
  @inline final def asOptional: POptional[S, T, A, B] =
    asScope asOptional

  /** View an [[EPScope]] as a Fold */
  @inline final def asFold: Fold[S, A] =
    asScope asFold

  /** View an [[EPScope]] as a PSetter */
  @inline final def asSetter: PSetter[S, T, A, B] =
    asScope asSetter

  /** View an [[EPScope]] as a PTraversal */
  @inline final def asTraversal: PTraversal[S, T, A, B] =
    asScope asTraversal
}

object EPScope {
  def id[S, T]: EPScope[Unit, S, T, S, T] =
    PIso.id[S, T].asEScope

  /** Create an [[EPScope]] using the canonical functions: getOrModify and put */
  def apply[E, S, T, A, B](
    _getOrModifyWithError: S => (E, T) \/ A
  )(
    _put: B => S => T
  ): EPScope[E, S, T, A, B] = new EPScope[E, S, T, A, B]{
    def getOrModifyWithError(s: S): (E, T) \/ A =
      _getOrModifyWithError(s)

    def put(b: B): S => T =
      _put(b)

    def getOrModify(s: S): T \/ A =
      getOrModifyWithError(s).leftMap(_._2)

    def getOrError(s: S): E \/ A =
      getOrModifyWithError(s).leftMap(_._1)

    def getOption(s: S): Option[A] =
      getOrModify(s).toOption

    def modify(f: A => B): S => T =
      s => getOrModify(s).fold(identity, a => put(f(a))(s))

    def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
      getOrModify(s).fold(
        t => Applicative[F].point(t),
        a => Applicative[F].map(f(a))(put(_)(s))
      )
  }
}

object EScope {
  def id[A]: EScope[Unit, A, A] =
    Iso.id[A].asEScope

  /** Alias for [[EPScope]] apply restricted to monomorphic update */
  def apply[E, S, A](_getOrError: S => E \/ A)(_put: A => S => S): EScope[E, S, A] =
    EPScope[E, S, S, A, A](
      s => _getOrError(s).fold(
        e => \/.left(e -> s),
        \/.right
      )
    )(
      _put
    )
}
