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

package au.com.cba.omnia.spectroscopy.law

import monocle.internal.IsEq
import scalaz.{\/, \/-, Id}, Id._

import au.com.cba.omnia.spectroscopy.EScope

class EScopeLaws[E, S, A](escope: EScope[E, S, A]) {
  import IsEq.syntax

  def getOrModifyWithErrorPut(s: S): IsEq[S] =
    escope.getOrModifyWithError(s).fold(_._2, escope.put(_)(s)) <==> s

  def putGetOrModifyWithError(s: S, a: A): IsEq[(E, S) \/ A] =
    escope.getOrModifyWithError(escope.put(a)(s)) <==> \/-(a)

  def putIdempotent(s: S, a: A, b: A): IsEq[S] =
    escope.put(a)(escope.put(b)(s)) <==> escope.put(a)(s)

  def getOrModify(s: S): IsEq[S \/ A] =
    escope.getOrModify(s) <==> escope.getOrModifyWithError(s).leftMap(_._2)

  def getOrError(s: S): IsEq[E \/ A] =
    escope.getOrError(s) <==> escope.getOrModifyWithError(s).leftMap(_._1)

  def getOption(s: S): IsEq[Option[A]] =
    escope.getOption(s) <==> escope.getOrModify(s).toOption

  def modifyIdentity(s: S): IsEq[S] =
    escope.modify(identity)(s) <==> s

  def modifyOptionIdentity(s: S): IsEq[Option[S]] =
    escope.modifyOption(identity)(s) <==> escope.getOption(s).map(_ => s)

  def modifyOrErrorIdentity(s: S): IsEq[E \/ S] =
    escope.modifyOrError(identity)(s) <==> escope.getOrError(s).map(_ => s)

  def modifyFId(s: S): IsEq[S] =
    escope.modifyF[Id](id.point[A](_))(s) <==> s
}
