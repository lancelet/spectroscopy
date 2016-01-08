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

import au.com.cba.omnia.spectroscopy.EPrism
import monocle.internal.IsEq

import scalaz.{\/, \/-, Id}, Id._

class EPrismLaws[E, S, A](eprism: EPrism[E, S, A]) {
  import IsEq.syntax

  def partialRoundTripOneWay(s: S): IsEq[S] =
    eprism.getOrModifyWithError(s).fold(_._2, eprism.reverseGet) <==> s
  
  def roundTripOtherWay(a: A): IsEq[(E, S) \/ A] =
    eprism.getOrModifyWithError(eprism.reverseGet(a)) <==> \/-(a)

  def modifyIdentity(s: S): IsEq[S] =
    eprism.modify(identity)(s) <==> s

  def modifyFId(s: S): IsEq[S] =
    eprism.modifyF[Id](id.point[A](_))(s) <==> s
  
  def modifyOptionIdentity(s: S): IsEq[Option[S]] =
    eprism.modifyOption(identity)(s) <==> eprism.getOption(s).map(_ => s)
  
  def modifyOrErrorIdentity(s: S): IsEq[E \/ S] =
    eprism.modifyOrError(identity)(s) <==> eprism.getOrError(s).map(_ => s)
}
