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

import au.com.cba.omnia.spectroscopy.Scope

class ScopeLaws[S, A](scope: Scope[S, A]) {
  import IsEq.syntax

  def getOrModifyPut(s: S): IsEq[S] =
    scope.getOrModify(s).fold(identity, scope.put(_)(s)) <==> s

  def putGetOrModify(s: S, a: A): IsEq[\/[S, A]] =
    scope.getOrModify(scope.put(a)(s)) <==> \/-(a)

  def putIdempotent(s: S, a: A, b: A): IsEq[S] =
    scope.put(a)(scope.put(b)(s)) <==> scope.put(a)(s)

  def getOption(s: S): IsEq[Option[A]] =
    scope.getOption(s) <==> scope.getOrModify(s).toOption

  def modifyIdentity(s: S): IsEq[S] =
    scope.modify(identity)(s) <==> s

  def modifyOptionIdentity(s: S): IsEq[Option[S]] =
    scope.modifyOption(identity)(s) <==> scope.getOption(s).map(_ => s)

  def modifyFId(s: S): IsEq[S] =
    scope.modifyF[Id](id.point[A](_))(s) <==> s
}
