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

package au.com.cba.omnia.spectroscopy.law.discipline

import monocle.law.discipline._
import org.scalacheck.{ Arbitrary, Prop }, Prop._
import org.typelevel.discipline.Laws
import scalaz.Equal
import scalaz.std.option._

import au.com.cba.omnia.spectroscopy.Scope
import au.com.cba.omnia.spectroscopy.law.ScopeLaws

object ScopeTests extends Laws {

  def apply[S: Arbitrary : Equal, A: Arbitrary : Equal](scope: Scope[S, A]): RuleSet = {
    val laws: ScopeLaws[S, A] = new ScopeLaws(scope)
    new SimpleRuleSet("Scope",
      "put what you get" -> forAll( (s: S)              => laws.getOrModifyPut(s)),
      "get what you put" -> forAll( (s: S, a: A)        => laws.putGetOrModify(s, a)),
      "put idempotent"   -> forAll( (s: S, a: A, b: A)  => laws.putIdempotent(s, a, b)),
      "getOption"        -> forAll( (s: S)              => laws.getOption(s)),
      "modifyOption"     -> forAll( (s: S)              => laws.modifyOptionIdentity(s)),
      "modify id = id"   -> forAll( (s: S)              => laws.modifyIdentity(s)),
      "modifyF id = id"  -> forAll( (s: S)              => laws.modifyFId(s))
    )
  }

}
