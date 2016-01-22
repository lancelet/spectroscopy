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
import scalaz.{\/, Applicative}

/**
 * A [[PScope]] can be seen as a pair of functions:
 *  - `getOrModify: S => T \/ A`
 *  - `put: B => S => T`
 *
 * A [[PScope]] could also be defined as a stronger POptional where the
 * setter method cannot fail.
 * 
 * Typically a [[PScope]] or [[Scope]] corresponds to the composition of a
 * PLens and a PPrism, thus encoding the relation between a Product of
 * CoProduct types (e.g. a map with case class values) and one of its elements.
 *
 * [[PScope]] stands for Polymorphic Spectroscope as its set and modify methods
 * change a type `A` to `B` and `S` to `T`.
 * [[Scope]] is a type alias for [[PScope]] where the type of target cannot be
 * modified:
 * {{{
 * type Scope[S, A] = PScope[S, S, A, A]
 * }}}
 *
 * A [[PScope]] is also a valid Fold, POptional, PTraversal, and
 * PSetter.
 *
 * @see [[au.com.cba.omnia.spectroscopy.law.ScopeLaws]]
 *
 * @tparam S the source of a [[PScope]]
 * @tparam T the modified source of a [[PScope]]
 * @tparam A the target of a [[PScope]]
 * @tparam B the modified target of a [[PScope]]
 *
 */
abstract class PScope[S, T, A, B] extends Serializable { self =>
  /** Get the target of a [[PScope]] or return the original value while
   *  allowing the type to change if it does not match */
  def getOrModify(s: S): T \/ A

  /** Put polymorphically the target of a [[PScope]] with a value */
  def put(b: B): S => T

  /** Get the target of a [[PScope]] or nothing if there is no target */
  def getOption(s: S): Option[A]

  /** Modify polymorphically the target of a [[PScope]] with a function */
  def modify(f: A => B): S => T

  /** Modify polymorphically the target of a [[PScope]] with an Applicative
   *  function */
  def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T]

  /** Set polymorphically the target of a [[PScope]] using a function */
  @inline final def set(b: B): S => T =
    modify(_ => b)

  /** 
   *  Modify polymorphically the target of a [[PScope]] with a function.
   *  Return nothing if the [[PScope]] is not matching
   */
  @inline final def modifyOption(f: A => B): S => Option[T] =
    s => getOption(s).map(a => put(f(a))(s))

  /**
   * Set polymorphically the target of a [[PScope]] with a value.
   * Return nothing if the [[PScope]] is not matching.
   */
  @inline final def setOption(b: B): S => Option[T] =
    modifyOption(_ => b)

  /** Check if a [[PScope]] has a target */
  @inline final def isMatching(s: S): Boolean =
    getOption(s).isDefined

  /** Join two [[PScope]] with the same target */
  @inline final def choice[S1, T1](other: PScope[S1, T1, A, B]): PScope[S \/ S1, T \/ T1, A, B] =
    PScope[S \/ S1, T \/ T1, A, B](
      _.fold(
        self.getOrModify(_) .leftMap(\/.left),
        other.getOrModify(_).leftMap(\/.right)
      )
    )(
      b => _.bimap(self.put(b), other.put(b))
    )

  @inline final def first[C]: PScope[(S, C), (T, C), (A, C), (B, C)] =
    PScope[(S, C), (T, C), (A, C), (B, C)]{
      case (s, c) => getOrModify(s).bimap(_ -> c, _ -> c)
    }{
      case (b, c) => {
        case (s, _) => (put(b)(s), c)
      }
    }

  @inline final def second[C]: PScope[(C, S), (C, T), (C, A), (C, B)] =
    PScope[(C, S), (C, T), (C, A), (C, B)]{
      case (c, s) => getOrModify(s).bimap(c -> _, c -> _)
    }{
      case (c, b) => {
        case (_, s) => (c, put(b)(s))
      }
    }

  /************************************************************/
  /** Compose methods between a [[PScope]] and another Optics */
  /************************************************************/

  /** Compose a [[PScope]] with a PIso */
  @inline final def composeIso[U, V](
    other: PIso[A, B, U, V]
  ): PScope[S, T, U, V] =
    self composePrism other.asPrism

  /** Compose a [[PScope]] with an [[EPPrism]] */
  @inline final def composeEPrism[E, U, V](
    other: EPPrism[E, A, B, U, V]
  ): EPScope[Option[E], S, T, U, V] =
    self.asEScope composeEPrismRight other

  /** Compose a [[PScope]] with a PPrism */
  @inline final def composePrism[U, V](
    other: PPrism[A, B, U, V]
  ): PScope[S, T, U, V] =
    PScope[S, T, U, V](
      s => getOrModify(s).flatMap(other.getOrModify(_).leftMap(put(_)(s)))
    )(
      u => s => put(other.reverseGet(u))(s)
    )

  /** Compose a [[PScope]] with a POptional */
  @inline final def composeOptional[U, V](
    other: POptional[A, B, U, V]
  ): POptional[S, T, U, V] =
    asOptional composeOptional other

  /** Compose a [[PScope]] with a Fold */
  @inline final def composeFold[C](other: Fold[A, C]): Fold[S, C] =
    asFold composeFold other

  /** Compose a [[PScope]] with a Getter */
  @inline final def composeGetter[C](other: Getter[A, C]): Fold[S, C] =
    asFold composeGetter other

  /** Compose a [[PScope]] with a PSetter */
  @inline final def composeSetter[U, V](
    other: PSetter[A, B, U, V]
  ): PSetter[S, T, U, V] =
    asSetter composeSetter other

  /** Compose a [[PScope]] with a PTraversal */
  @inline final def composeTraversal[U, V](
    other: PTraversal[A, B, U, V]
  ): PTraversal[S, T, U, V] =
    asTraversal composeTraversal other

  /** Compose a [[PScope]] with a PLens */
  @inline final def composeLens[U, V](
    other: PLens[A, B, U, V]
  ): POptional[S, T, U, V] =
    asOptional composeOptional other.asOptional

  /*
   *  Assuming that self satisfies the ScopeLaws getOrModifyPut,
   *  putGetOrModify, and putIdempotent, and assuming that other satisfies the
   *  PrismLaws for partialRoundTripOneWay, and roundTripOtherway, then the
   *  Scope given by composePrism will satisfy the ScopeLaws.
   *  Note that we need only show the ScopeLaws getOrModifyPut,
   *  putGetOrModify, and putIdempotent, as the other ScopeLaws are
   *  guaranteed by the definition of PScope.apply.
   * 
   *  -----------------------------------------------------------------------
   *
   *  Proposition:
   *    ScopeLaws.getOrModifyPut holds. That is:
   *      _getOrModify(s).fold(identity, _put(_)(s)) === s
   *
   *  Proof:
   *    Suppose that getOrModify(s) === \/-(a) and other.getOrModify(a)
   *    \/-(u) for some (a: A, u: U).
   *
   *    (1) _getOrModify(s)
   *    (2) getOrModify(s).flatMap(other.getOrModify(_).leftMap(put(_)(s)))
   *    (3) \/-(a).flatMap(other.getOrModify(_).leftMap(put(_)(s)))
   *    (4) other.getOrModify(a).leftMap(put(_)(s))
   *    (5) \/-(u).leftMap(put(_)(s))
   *    (6) \/-(u)
   *
   *    (1) === (2) from the definition of _getOrModify.
   *    (2) === (3) from hypothesis.
   *    (3) === (4) from from the definition of \/.flatMap.
   *    (4) === (5) from hypothesis.
   *    (5) === (6) from the definition of \/.leftMap.
   *
   *    Then:
   *
   *    (1)  _getOrModify(s).fold(identity, _put(_)(s))
   *    (2)  \/-(u).fold(identity, _put(_)(s))
   *    (3)  _put(u)(s)
   *    (4)  put(other.reverseGet(u))(s)
   *    (5)  put(\/-(u).fold(identity, other.reverseGet))(s)
   *    (6)  put(other.getOrModify(a).fold(identity, other.reverseGet))(s)
   *    (7)  put(a)(s)
   *    (8)  \/-(a).fold(identity, put(_)(s))
   *    (9)  getOrModify(s).fold(identity, put(_)(s))
   *    (10) s
   *
   *    (1) === (2)  from above.
   *    (2) === (3)  from the definition of \/.fold.
   *    (3) === (4)  from the definition of _put.
   *    (4) === (5)  from the definition of \/.fold.
   *    (5) === (6)  from hypothesis.
   *    (6) === (7)  from PrismLaws.partialRoundTripOneWay.
   *    (8) === (9)  from hypothesis.
   *    (9) === (10) from ScopeLaws.getOrModifyPut.
   *
   *    Suppose instead that getOrModify(s) === -\/(s).
   *
   *    (1) _getOrModify(s)
   *    (2) -\/(s).flatMap(other.getOrModify(_).leftMap(put(_)(s)))
   *    (3) -\/(s)
   *
   *    (1) === (2) from the definition of _getOrModify.
   *    (2) === (3) from hypothesis.
   *
   *    Then:
   *
   *    (1) _getOrModify(s).fold(identity, _put(_)(s))
   *    (2) -\/(s).fold(identity, _put(_)(s))
   *    (3) identity(s)
   *    (4) s
   *
   *    (1) === (2) from above.
   *    (2) === (3) from the definition of \/.fold.
   *    (3) === (4) from the definition of identity.
   *
   *    Suppose instead that getOrModify(s) === \/-(a) and
   *    other.getOrModify(a) === -\/(a) for some (a: A).
   *
   *    (1) _getOrModify(s)
   *    (2) \/-(a).flatMap(other.getOrModify(_).leftMap(put(_)(s)))
   *    (3) other.getOrModify(a).leftMap(put(_)(s))
   *    (4) -\/(a).leftMap(put(_)(s))
   *    (5) -\/(put(a)(s))
   *    (6) -\/(\/-(a).fold(identity, put(_)(s)))
   *    (7) -\/(getOrModify(s).fold(identity, put(_)(s)))
   *    (8) -\/(s)
   *
   *    Then _getOrModify(s).fold(identity, _put(_)(s)) === s, from the same
   *    reasoning as in the previous case.
   *
   *  Proposition:
   *    ScopeLaws.putGetOrModify holds. That is:
   *      _getOrModify(_put(u)(s)) === \/-(u)
   *
   *  Proof:
   *    (1) _getOrModify(_put(u)(s))
   *    (2) getOrModify(_put(u)(s))
   *          .flatMap(other.getOrModify(_).leftMap(put(_)(s)))
   *    (3) getOrModify(put(other.reverseGet(u))(s))
   *          .flatMap(other.getOrModify(_).leftMap(put(_)(s)))
   *    (4) \/-(other.reverseGet(u))
   *          .flatMap(other.getOrModify(_).leftMap(put(_)(s)))
   *    (5) other.getOrModify(other.reverseGet(u))
   *          .leftMap(put(_)(s))
   *    (6) \/-(u).leftMap(put(_)(s))
   *    (7) \/-(u)
   *
   *    (1) === (2) from the definition of _getOrModify.
   *    (2) === (3) from the definition of _put.
   *    (3) === (4) from ScopeLaws.putGetOrModify.
   *    (4) === (5) from the definition of \/.flatMap.
   *    (5) === (6) from PrismLaws.roundTripOtherWay.
   *    (6) === (7) from the definition of \/.leftMap.
   *  
   *  Proposition:
   *    ScopeLaws.putIdempotent holds. That is:
   *      _put(u)(_put(v)(s)) === _put(u)(s)
   *
   *  Proof:
   *    (1) _put(u)(_put(v)(s))
   *    (2) put(other.reverseGet(u))(_put(v)(s))
   *    (3) put(other.reverseGet(u))(put(other.reverseGet(v))(s))
   *    (4) put(other.reverseGet(u))(s)
   *    (5) _put(u)(s)
   *
   *    (1) === (2) from the definition of _put.
   *    (2) === (3) from the definition of _put.
   *    (3) === (4) from ScopeLaws.putIdempotent.
   */

  /******************************************************************/
  /** Transformation methods to view a [[PScope]] as another Optics */
  /******************************************************************/

  /** View a [[PScope]] as an [[EPScope]] */
  @inline final def asEScope: EPScope[Unit, S, T, A, B] =
    EPScope[Unit, S, T, A, B](getOrModify(_).leftMap(() -> _))(put(_))

  /** View a [[PScope]] as a POptional */
  @inline final def asOptional: POptional[S, T, A, B] =
    POptional[S, T, A, B](getOrModify(_))(set(_))

  /** View a [[PScope]] as a Fold */
  @inline final def asFold: Fold[S, A] =
    asOptional asFold

  /** View a [[PScope]] as a PSetter */
  @inline final def asSetter: PSetter[S, T, A, B] =
    new PSetter[S, T, A, B] {
      def modify(f: A => B): S => T =
        self.modify(f)

      def set(b: B): S => T =
        self.put(b)
    }

  /** View a [[PScope]] as a PTraversal */
  @inline final def asTraversal: PTraversal[S, T, A, B] =
    asOptional asTraversal
}

object PScope {
  def id[S, T]: PScope[S, T, S, T] =
    PIso.id[S, T].asScope

  /** Create a [[PScope]] using the canonical functions: getOrModify and put */
  def apply[S, T, A, B](
    _getOrModify: S => T \/ A
  )(
    _put: B => S => T
  ): PScope[S, T, A, B] = new PScope[S, T, A, B]{
    def getOrModify(s: S): T \/ A =
      _getOrModify(s)

    def put(b: B): S => T =
      _put(b)

    def getOption(s: S): Option[A] =
      getOrModify(s).toOption

    def modify(f: A => B): S => T =
      s => getOrModify(s).fold(identity, a => put(f(a))(s))

    def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] =
      getOrModify(s).fold(
        t => Applicative[F].point(t),
        a => Applicative[F].map(f(a))(put(_)(s))
      )

    /*
     *  Assuming that _getOrModify and _put satisfy the ScopeLaws for
     *  getOrModifyPut, putGetOrModify, and putIdempotent, then the
     *  Scope defined here will satisfy the remaining ScopeLaws.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *   ScopeLaws.getOption holds. That is:
     *
     *    getOption(s) === getOrModify(s).toOption
     * 
     *  Proof:
     *    From the definition of getOption.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.modifyIdentity holds. That is:
     *
     *      modify(identity)(s) === s
     *  
     *  Proof:
     *    (1) modify(identity)(s)
     *    (2) getOrModify(s).fold(identity, a => put(identity(a))(s))
     *    (3) getOrModify(s).fold(identity, a => put(a)(s))
     *    (4) getOrModify(s).fold(identity, put(_)(s))
     *    (5) s
     *
     *    (1) === (2) from the definition of modify.
     *    (2) === (3) from the definition identity.
     *    (3) === (4) from the definition of placeholder syntax.
     *    (4) === (5) from SpectroScopeLaws.getOrModifyPut.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.modifyOptionIdentity holds. That is:
     *      modifyOption(identity)(s) === getOption(s).map(_ => s)
     *
     *  Proof:
     *    Suppose that: getOption(s) === Some(a) for some (a: A).
     *
     *    By definition: getOption(s) === getOrModify(s).toOption
     *    So: getOrModify(s) === \/-(a)
     *
     *    (1)  modifyOption(identity)(s)
     *    (2)  getOption(s).map(a => put(identity(a))(s))
     *    (3)  getOrModify(s).toOption.map(a => put(identity(a))(s))
     *    (4)  getOrModify(s).map(a => put(identity(a))(s)).toOption
     *    (5)  getOrModify(s)
     *           .map(a => put(identity(a))(s))
     *           .fold(_ => None, Some(_))
     *    (6)  getOrModify(s)
     *           .fold(_ => None, a => Some(put(identity(a))(s)))
     *    (7)  \/-(a)
     *           .fold(_ => None, a => Some(put(identity(a))(s)))
     *    (8)  \/-(a)
     *           .fold(_ => Some(_), a => Some(put(identity(a))(s)))
     *    (9)  Some(\/-(a).fold(_ => _, a => put(identity(a))(s)))
     *    (10) Some(\/-(a).fold(_ => _, a => put(a)(s)))
     *    (11) Some(\/-(a).fold(identity, a => put(a)(s)))
     *    (12) Some(\/-(a).fold(identity, put(_)(s)))
     *    (13) Some(getOrModify(s).fold(identity, put(_)(s)))
     *    (14) Some(s)
     *    (15) Some(a).map(_ => s)
     *    (16) getOption(s).map(_ => s)
     *
     *    (1)  === (2)  from the definition of modifyOption.
     *    (2)  === (3)  from the definition of getOption.
     *    (3)  === (4)  as \/.toOption.map === \/.map.toOption.
     *    (4)  === (5)  from the definition of \/.toOption and \/.fold.
     *    (5)  === (6)  from the definition of \/.map and \/.fold.
     *    (6)  === (7)  from hypothesis.
     *    (7)  === (8)  from the definition of \/.fold.
     *    (8)  === (9)  from the definition of \/.fold.
     *    (9)  === (10) from the definition of identity.
     *    (10) === (11) from the definition of identity.
     *    (11) === (12) from the definition of placeholder syntax.
     *    (12) === (13) from hypothesis.
     *    (13) === (14) from ScopeLaws.getOrModifyPut.
     *    (14) === (15) from the definition of Option.map.
     *    (15) === (16) from hypothesis.
     *    
     *    Suppose instead that: getOption(s) === None.
     *
     *    (1) modifyOption(identity)(s)
     *    (2) getOption(s).map(a => put(identity(a))(s))
     *    (3) None.map(a => put(identity(a))(s))
     *    (4) None
     *    (5) None.map(_ => s)
     *    (6) getOption(s).map(_ => s)
     *
     *    (1) === (2) from the definition of modifyOption.
     *    (2) === (3) from hypothesis.
     *    (3) === (4) from the definition of Option.map
     *    (4) === (5) from the definition of Option.map
     *    (5) === (6) from hypothesis.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.modifyFId holds. That is:
     *      modifyF[Id](id.point[A](_))(s) === s
     *
     *  Proof:
     *    (1) modifyF[Id](id.point[A](_))(s)
     *    (2) getOrModify(s).fold(
     *          t => Applicative[Id].point(t),
     *          a => Applicative[Id].map(identity(a))(put(_)(s))
     *        )
     *    (3) getOrModify(s).fold(
     *          identity,
     *          put(identity(_))(s)
     *        )
     *    (4) getOrModify(s).fold(identity, put(_)(s))
     *    (5) s
     *
     *    (1) === (2) from the definition of modifyF
     *    (2) === (3) from the definition of Applicative[Id].point,
     *                Applicative[Id].map, and identity
     *    (3) === (4) from the definition of identity
     *    (4) === (5) from ScopeLaws.getOrModifyPut
     */
  }
}

object Scope {
  def id[A]: Scope[A, A] =
    Iso.id[A].asScope

  /** Alias for [[PScope]] apply restricted to monomorphic update */
  def apply[S, A](_getOption: S => Option[A])(_put: A => S => S): Scope[S, A] =
    PScope[S, S, A, A](
      s => _getOption(s).fold[S \/ A](\/.left(s))(\/.right)
    )(
      _put
    )
}
