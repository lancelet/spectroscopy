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

package au.com.cba.omnia

import monocle.{
  Fold,
  Getter,
  PIso,
  PLens,
  POptional,
  PPrism,
  PTraversal,
  PSetter
}
import scalaz.{\/-}

package object spectroscopy {
  type Scope[S, A] = PScope[S, S, A, A]

  implicit class RichIso[S, T, A, B](iso: PIso[S, T, A, B]) {
    /** View a PIso as a [[PScope]] */
    def asScope: PScope[S, T, A, B] =
      PScope[S, T, A, B](
        s => \/-(iso.get(s))
      )(
        b => _ => iso.reverseGet(b)
      )

    /*
     *  Assuming that iso satisfies the IsoLaws roundTripOneWay and
     *  roundTripOtherWay, then the Scope given by asScope will
     *  satisfy the ScopeLaws. Note that we need only show the
     *  ScopeLaws getOrModifyPut, putGetOrModify, and putIdempotent, as
     *  the other ScopeLaws are guaranteed by the definition of
     *  PScope.apply.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.getOrModifyPut holds. That is:
     *      _getOrModify(s).fold(identity, _put(_)(s)) === s
     *
     *  Proof:
     *    (1) _getOrModify(s).fold(identity, _put(_)(s))
     *    (2) \/-(iso.get(s)).fold(identity, _put(_)(s))
     *    (3) _put(iso.get(s))(s)
     *    (4) iso.reverseGet(iso.get(s))
     *    (5) s
     *
     *    (1) === (2) from the definition of _getOrModify.
     *    (2) === (3) from the definition of \/.fold.
     *    (3) === (4) from the definition of _put.
     *    (4) === (5) from IsoLaws.roundTripOneWay.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.putGetOrModify holds. That is:
     *      _getOrModify(_put(a)(s)) === \/-(a)
     *
     *  Proof:
     *    (1) _getOrModify(_put(a)(s))
     *    (2) \/-(iso.get(_put(a)(s)))
     *    (3) \/-(iso.get(iso.reverseGet(a)))
     *    (4) \/-(a)
     *
     *    (1) === (2) from the definition of _getOrModify.
     *    (2) === (3) from the definition of _put.
     *    (3) === (4) from IsoLaws.roundTripOtherWay.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.putIdempotent holds. That is:
     *      _put(a)(_put(b)(s)) === _put(a)(s)
     *
     *  Proof:
     *    (1) _put(a)(_put(b)(s))
     *    (2) iso.reverseGet(a)
     *    (3) _put(a)(s)
     *    
     *    (1) === (2) from the definition of _put.
     *    (2) === (3) from the definition of _put.
     */

    /** Compose a PIso with a [[PScope]] */
    def composeScope[U, V](other: PScope[A, B, U, V]) =
      iso.asLens composeScope other
  }

  implicit class RichLens[S, T, A, B](lens: PLens[S, T, A, B]) {
    /** View a PLens as a [[PScope]] */
    def asScope =
      lens composeScope PScope.id[A, B]

    /** Compose a PLens with a [[PScope]] */
    def composeScope[U, V](other: PScope[A, B, U, V]) =
      PScope[S, T, U, V](
        s => other.getOrModify(lens.get(s)).leftMap(lens.set(_)(s))
      )(
        v => s => lens.set(other.put(v)(lens.get(s)))(s)
      )
    /*
     *  Assuming that lens satisfies the LensLaws getSet, setGet, and
     *  setIdempotent, and assuming that other satisfies the ScopeLaws
     *  getOrModifyPut, putGetOrModify, and putIdempotent, then the
     *  Scope given by composeScope will satisfy the
     *  ScopeLaws. Note that we need only show the ScopeLaws
     *  getOrModifyPut, putGetOrModify, and putIdempotent, as the other
     *  ScopeLaws are guaranteed by the definition of
     *  PScope.apply.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.getOrModifyPut holds. That is:
     *      _getOrModify(s).fold(identity, _put(_)(s)) === s
     *
     *  Proof:
     *    Suppose that other.getOrModify(lens.get(s)) === \/-(u) for some
     *    (u: U).
     *
     *    (1)  _getOrModify(s).fold(identity, _put(_)(s))
     *    (2)  other
     *           .getOrModify(lens.get(s))
     *           .leftMap(lens.set(_)(s))
     *           .fold(identity, _put(_)(s))
     *    (3)  \/-(u)
     *           .leftMap(lens.set(_)(s))
     *           .fold(identity, _put(_)(s))
     *    (4)  \/-(u).fold(identity, _put(_)(s))
     *    (5)  _put(u)(s)
     *    (6)  lens.set(other.put(u)(lens.get(s)))(s)
     *    (7)  lens.set(
     *           \/-(u).fold(identity, other.put(_)(lens.get(s)))
     *         )(s)
     *    (8)  lens.set(
     *           other
     *             .getOrModify(lens.get(s))
     *             .fold(identity, other.put(_)(lens.get(s)))
     *         )(s)
     *    (9)  lens.set(lens.get(s))(s)
     *    (10) s
     *
     *    (1) === (2)  from the definition of _getOrModify.
     *    (2) === (3)  from hypothesis.
     *    (3) === (4)  from the definition of \/.leftMap.
     *    (4) === (5)  from the definition of \/.fold.
     *    (5) === (6)  from the definition of _put.
     *    (6) === (7)  from the definition of \/.fold.
     *    (7) === (8)  from hypothesis.
     *    (8) === (9)  from ScopeLaws.getOrModifyPut.
     *    (9) === (10) from LensLaws.getSet.
     *
     *    Suppose instead that other.getOrModify(lens.get(s)) === -\/(a) for
     *    some (a: A). Note that other.getOrModify(a: A): A \/ U. Therefore
     *    lens.get(s) === a.
     *
     *    (1) _getOrModify(s).fold(identity, _put(_)(s))
     *    (2) other
     *          .getOrModify(lens.get(s))
     *          .leftMap(lens.set(_)(s))
     *          .fold(identity, _put(_)(s))
     *    (3) -\/(a)
     *          .leftMap(lens.set(_)(s))
     *          .fold(identity, _put(_)(s))
     *    (4) -\/(lens.set(a)(s)).fold(identity, _put(_)(s))
     *    (5) lens.set(a)(s)
     *    (6) lens.set(lens.get(s))(s)
     *    (7) s
     *
     *    (1) === (2) from the definition of _getOrModify.
     *    (2) === (3) from hypothesis.
     *    (3) === (4) from the definition of \/.leftMap.
     *    (4) === (5) from the definition of \/.fold.
     *    (5) === (6) from above.
     *    (6) === (7) from LensLaws.getSet.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.putGetOrModify holds. That is:
     *      _getOrModify(_put(u)(s)) === \/-(u)
     */
    /*  Proof:
     *    (1) _getOrModify(_put(u)(s))
     *    (2) other
     *          .getOrModify(lens.get(_put(u)(s)))
     *          .leftMap(lens.set(_)(_put(u)(s)))
     *    (3) other
     *          .getOrModify(
     *            lens.get(lens.set(other.put(u)(lens.get(s)))(s))
     *          )
     *          .leftMap(lens.set(_)(_put(u)(s)))
     *    (4) other
     *          .getOrModify(other.put(u)(lens.get(s)))
     *          .leftMap(lens.set(_)(_put(u)(s)))
     *    (5) \/-(u).leftMap(lens.set(_)(_put(u)(s)))
     *    (6) \/-(u)
     */
    /*    (1) === (2) from the definition of _getOrModify.
     *    (2) === (3) from the definition of _put.
     *    (3) === (4) from LensLaws.setGet.
     *    (4) === (5) from ScopeLaws.putGetOrModify.
     *    (5) === (6) from the definition of \/.leftMap.
     *
     *  ---------------------------------------------------------------------
     *
     *  Proposition:
     *    ScopeLaws.putIdempotent holds. That is:
     *      _put(u)(_put(v)(s)) === _put(u)(s)
     *
     *  Proof:
     *    (1) _put(u)(_put(v)(s))
     *    (2) lens.set(
     *          other.put(u)(
     *            lens.get(lens.set(other.put(v)(lens.get(s)))(s))
     *          )
     *        )(
     *          _put(v)(s)
     *        )
     *    (3) lens.set(
     *          other.put(u)(
     *            lens.get(lens.set(other.put(v)(lens.get(s)))(s))
     *          )
     *        )(
     *          lens.set(other.put(v)(lens.get(s)))(s)
     *        )
     *    (4) lens.set(
     *          other.put(u)(
     *            lens.get(lens.set(other.put(v)(lens.get(s)))(s))
     *          )
     *        )(s)
     *    (5) lens.set(other.put(u)(other.put(v)(lens.get(s))))(s)
     *    (6) lens.set(other.put(u)(lens.get(s)))(s)
     *    (7) _put(u)(s)
     *
     *    (1) === (2) from the definition of _put.
     *    (2) === (3) from the definition of _put.
     *    (3) === (4) from Lens.setIdempotent.
     *    (4) === (5) from Lens.setGet.
     *    (5) === (6) from Scope.putIdempotent.
     *    (6) === (7) from the definition of _put.
     */
  }

  implicit class RichPrism[S, T, A, B](prism: PPrism[S, T, A, B]) {
    /** View a PPrism as a [[PScope]] */
    def asScope =
      PScope.id[S, T] composePrism prism

    /** Compose a PPrism with a [[PScope]] */
    def composeScope[U, V](other: PScope[A, B, U, V]): POptional[S, T, U, V] =
      prism composeOptional other.asOptional
  }

  implicit class RichOptional[S, T, A, B](optional: PPrism[S, T, A, B]) {
    /** Compose a POptional with a [[PScope]] */
    def composeScope[U, V](other: PScope[A, B, U, V]): POptional[S, T, U, V] =
      optional composeOptional other.asOptional
  }

  implicit class RichTraversal[S, T, A, B](traversal: PTraversal[S, T, A, B]) {
    /** Compose a PTraversal with a [[PScope]] */
    def composeScope[U, V](other: PScope[A, B, U, V]): PTraversal[S, T, U, V] =
      traversal composeTraversal other.asTraversal
  }

  implicit class RichFold[S, A](fold: Fold[S, A]) {
    /** Compose a Fold with a [[PScope]] */
    def composeScope[B, U, V](other: PScope[A, B, U, V]): Fold[S, U] =
      fold composeFold other.asFold
  }

  implicit class RichGetter[S, A](getter: Getter[S, A]) {
    /** Compose a Getter with a [[PScope]] */
    def composeScope[B, U, V](other: PScope[A, B, U, V]): Fold[S, U] =
      getter.asFold composeFold other.asFold
  }

  implicit class RichSetter[S, T, A, B](setter: PSetter[S, T, A, B]) {
    /** Compose a PSetter with a [[PScope]] */
    def composeScope[U, V](other: PScope[A, B, U, V]): PSetter[S, T, U, V] =
      setter composeSetter other.asSetter
  }
}
