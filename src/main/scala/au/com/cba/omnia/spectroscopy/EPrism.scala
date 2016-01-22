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
 * An [[EPPrism]] can be seen as a pair of functions:
 *  - `getOrModifyWithError: S => (E, T) \/ A`
 *  - `reverseGet          : B => T`
 *
 * [[EPrism]] stands for Error-reporting Prism.
 *
 * An [[EPPrism]] could also be defined as a PPrism where the getter method
 * returns an error message if the PPrism isn't matching.
 *
 * [[EPrism]] is a type alias for [[EPPrism]] where the type of a target cannot
 * be modified:
 * {{{
 * type EPrism[E, S, A] = EPPrism[E, S, S, A, A]
 * }}}
 *
 * An [[EPPrism]] is also a valid PPrism, and in turn a valid Fold,
 * POptional, PTraversal and PSetter.
 *
 * @see [[au.com.cba.omnia.spectroscopy.law.EPrismLaws]]
 *
 * @tparam E the error message returned by a [[EPPrism]]
 * @tparam S the source of a [[EPPrism]]
 * @tparam T the modified source of a [[EPPrism]]
 * @tparam A the target of a [[EPPrism]]
 * @tparam B the modified target of a [[EPPrism]]
 */
abstract class EPPrism[E, S, T, A, B] extends Serializable { self =>
  /** get the target of an [[EPPrism]] or return an error message and the
   *  original value, while allowing the type to change if it does not match */
  def getOrModifyWithError(s: S): (E, T) \/ A

  /** get the modified source of an [[EPPrism]] */
  def reverseGet(b: B): T

  /** get the target of an [[EPPrism]] or return the original value while
   *  allowing the type to change if it does not match */
  def getOrModify(s: S): T \/ A

  /** get the target of an [[EPPrism]] or an error message if there is no
   *  target */
  def getOrError(s: S): E \/ A

  /** get the target of an [[EPPrism]] or nothing if there is no target */
  def getOption(s: S): Option[A]

  /** modify polymorphically the target of an [[EPPrism]] with a Applicative
   *  function */
  @inline final def modifyF[F[_] : Applicative](f: A => F[B])(s: S): F[T] =
    getOrModify(s).fold(
      t => Applicative[F].point(t),
      a => Applicative[F].map(f(a))(reverseGet)
    )

  /** modify polymorphically the target of an [[EPPrism]] with a function */
  @inline final def modify(f: A => B): S => T =
    getOrModify(_).fold(identity, a => reverseGet(f(a)))

  /** modify polymorphically the target of an [[EPPrism]] with a function.
   *  return an error message if the [[EPPrism]] is not matching */
  @inline final def modifyOrError(f: A => B): S => E \/ T =
    s => getOrError(s).map(a => reverseGet(f(a)))

  /** modify polymorphically the target of an [[EPPrism]] with a function.
   *  return nothing if the [[EPPrism]] is not matching */
  @inline final def modifyOption(f: A => B): S => Option[T] =
    modifyOrError(f)(_).toOption

  /** set polymorphically the target of an [[EPPrism]] with a value. */
  @inline final def set(b: B): S => T =
    modify(_ => b)

  /** set polymorphically the target of an [[EPPrism]] with a value. 
   * return an error message if the [[EPPrism]] is not matching */
  @inline final def setOrError(b: B): S => E \/ T =
    modifyOrError(_ => b)

  /** set polymorphically the target of an [[EPPrism]] with a value. 
   * return nothing if the [[EPPrism]] is not matching */
  @inline final def setOption(b: B): S => Option[T] =
    modifyOption(_ => b)

  /** check if an [[EPPrism]] has a target */
  @inline final def isMatching(s: S): Boolean =
    getOption(s).isDefined

  /** create a Getter from the modified target to the modified source of an
   *  [[EPPrism]] */
  @inline final def re: Getter[B, T] =
    asPrism.re

  @inline final def first[C]: EPPrism[E, (S, C), (T, C), (A, C), (B, C)] =
    EPPrism[E, (S, C), (T, C), (A, C), (B, C)]{
      case (s, c) => getOrModifyWithError(s).bimap(
        et => et.copy(_2 = et._2 -> c),
        _ -> c
      )
    }{
      case (b, c) => (reverseGet(b), c)
    }

  @inline final def second[C]: EPPrism[E, (C, S), (C, T), (C, A), (C, B)] =
    EPPrism[E, (C, S), (C, T), (C, A), (C, B)]{
      case (c, s) => getOrModifyWithError(s).bimap(
        et => et.copy(_2 = c -> et._2),
        c -> _
      )
    }{
      case (c, b) => (c, reverseGet(b))
    }

  @inline final def left[C] : EPPrism[E, S \/ C, T \/ C, A \/ C, B \/ C] =
    EPPrism[E, S \/ C, T \/ C, A \/ C, B \/ C](
    _.fold(
      getOrModifyWithError(_).bimap(
        et => et.copy(_2 = -\/(et._2)),
        \/.left
      ),
      c => \/.right(\/.right(c))
    )
    )(_.leftMap(reverseGet))

  @inline final def right[C]: EPPrism[E, C \/ S, C \/ T, C \/ A, C \/ B] =
    EPPrism[E, C \/ S, C \/ T, C \/ A, C \/ B](
    _.fold(
      c => \/.right(\/.left(c)),
      getOrModifyWithError(_).bimap(
        et => et.copy(_2 = \/-(et._2)),
        \/.right
      )
    )
    )(_.map(reverseGet))

  @inline final def mapError[F](f: E => F): EPPrism[F, S, T, A, B] =
    EPPrism[F, S, T, A, B](
      getOrModifyWithError(_).leftMap(et =>
        et.copy(_1 = f(et._1))
      )
    )(reverseGet)

  /**************************************************************/
  /** Compose methods between an [[EPPrism]] and another Optics */
  /**************************************************************/

  /** compose an [[EPPrism]] with a Fold */
  @inline final def composeFold[C](other: Fold[A, C]): Fold[S, C] =
    asPrism composeFold other

  /** compose a [[EPPrism]] with a Getter */
  @inline final def composeGetter[C](other: Getter[A, C]): Fold[S, C] =
    asPrism composeGetter other

  /** compose a [[EPPrism]] with a PSetter */
  @inline final def composeSetter[C, D](
    other: PSetter[A, B, C, D]
  ): PSetter[S, T, C, D] =
    asPrism composeSetter other

  /** compose a [[EPPrism]] with a PTraversal */
  @inline final def composeTraversal[C, D](
    other: PTraversal[A, B, C, D]
  ): PTraversal[S, T, C, D] =
    asPrism composeTraversal other

  /** compose a [[EPPrism]] with a POptional */
  @inline final def composeOptional[C, D](
    other: POptional[A, B, C, D]
  ): POptional[S, T, C, D] =
    asPrism composeOptional other

  /** compose a [[EPPrism]] with a PLens */
  @inline final def composeLens[C, D](
    other: PLens[A, B, C, D]
  ): POptional[S, T, C, D] =
    asPrism composeOptional other.asOptional

  /** compose a [[EPPrism]] with an [[EPPrism]] */
  @inline final def composeEPrism[F, C, D](
    other: EPPrism[F, A, B, C, D]
  ): EPPrism[E \/ F, S, T, C, D] =
    EPPrism[E \/ F, S, T, C, D](
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
      d => self.reverseGet(other.reverseGet(d))
    )

  @inline final def composeEPrismLeft[F, U, V](
    other: EPPrism[F, A, B, U, V]
  ): EPPrism[Option[E], S, T, U, V] =
    (self composeEPrism other).leftError

  @inline final def composeEPrismRight[F, U, V](
    other: EPPrism[F, A, B, U, V]
  ): EPPrism[Option[F], S, T, U, V] =
    (self composeEPrism other).rightError

  /** Compose an [[EPPrism]] with a PPrism */
  @inline final def composePrism[U, V](
    other: PPrism[A, B, U, V]
  ): EPPrism[Option[E], S, T, U, V] =
    self composeEPrismLeft other.asEPrism

  /** compose an [[EPPrism]] with a PIso */
  @inline final def composeIso[C, D](
    other: PIso[A, B, C, D]
  ): EPPrism[Option[E], S, T, C, D] =
    self composeEPrismLeft other.asEPrism
  
  /********************************************************************/
  /** Transformation methods to view an [[EPPrism]] as another Optics */
  /********************************************************************/

  /** view an [[EPPrism]] as a PPrism */
  @inline final def asPrism: PPrism[S, T, A, B] = PPrism[S, T, A, B](
    getOrModify
  )(
    reverseGet
  )

  /** view an [[EPPrism]] as a Fold */
  @inline final def asFold: Fold[S, A] =
    asPrism asFold

  /** view an [[EPPrism]] as a Setter */
  @inline final def asSetter: PSetter[S, T, A, B] =
    asPrism asSetter

  /** view an [[EPPrism]] as a PTraversal */
  @inline final def asTraversal: PTraversal[S, T, A, B] =
    asPrism asTraversal

  /** view an [[EPPrism]] as a POptional */
  @inline final def asOptional: POptional[S, T, A, B] =
    asPrism asOptional

  /** View an EPPrism as a [[PScope]] */
  def asScope: PScope[S, T, A, B] =
    PIso.id[S, T] composePrismAsScope self.asPrism

  /** View an EPPrism as an [[EPScope]] */
  def asEScope: EPScope[E, S, T, A, B] =
    EPScope[E, S, T, A, B](
      s => getOrModifyWithError(s)
    )(
      b => _ => reverseGet(b)
    )
}

object EPPrism {
  def id[S, T]: EPPrism[Unit, S, T, S, T] =
    PIso.id[S, T].asEPrism

  def apply[E, S, T, A, B](
    _getOrModifyWithError: S => (E, T) \/ A
  )(
    _reverseGet: B => T
  ): EPPrism[E, S, T, A, B] =
    new EPPrism[E, S, T, A, B] {
      def getOrModifyWithError(s: S): (E, T) \/ A =
        _getOrModifyWithError(s)

      def reverseGet(b: B): T =
        _reverseGet(b)

      def getOrModify(s: S): T \/ A =
        getOrModifyWithError(s).leftMap(_._2)

      def getOrError(s: S): E \/ A =
        getOrModifyWithError(s).leftMap(_._1)

      def getOption(s: S): Option[A] =
        getOrModifyWithError(s).toOption
    }
}

object EPrism {
  /** Alias for [[EPPrism]] apply restricted to monomorphic update */
  def apply[E, S, A](
    _getOrError: S => E \/ A
  )(
    _reverseGet: A => S
  ): EPrism[E, S, A] =
    EPPrism[E, S, S, A, A](
      s => _getOrError(s).fold[(E, S) \/ A](e => \/.left(e -> s), \/.right)
    )(
      _reverseGet
    )
}
