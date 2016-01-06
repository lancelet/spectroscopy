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

import org.scalacheck.Arbitrary, Arbitrary._
import scalaz._, Scalaz._

import au.com.cba.omnia.spectroscopy.law.discipline.ScopeTests

class ScopeSpec extends SpectroscopySuite {
  // Iso asScope
  checkAll("tuple2Reverse.reverse.asScope",
    ScopeTests(tuple2Reverse[Int, String].reverse.asScope))

  // Lens asScope
  checkAll("tuple2Field1.first.asScope",
    ScopeTests(tuple2Field1[Int, String].first.asScope)
  )
  checkAll("tuple2Field2.second.asScope",
    ScopeTests(tuple2Field2[Int, String].second.asScope)
  )

  // Prism asScope
  checkAll("some.asScope",
    ScopeTests(some[Int].asScope)
  )
  checkAll("none.asScope",
    ScopeTests(none[Int].asScope)
  )
  checkAll("stdLeft.asScope",
    ScopeTests(stdLeft[Int, String].asScope)
  )
  checkAll("stdRight.asScope",
    ScopeTests(stdRight[Int, String].asScope)
  )

  // Lens composeScope
  checkAll("(tuple2Field1.first composeScope some.asScope)",
    ScopeTests(
      tuple2Field1[Option[Int], String].first
        composeScope some[Int].asScope
    )
  )
  checkAll("(tuple2Field2.second composeScope some.asScope)",
    ScopeTests(
      tuple2Field2[Int, Option[String]].second
        composeScope some[String].asScope
    )
  )

  // Scope composePrism
  checkAll("(tuple2Field1.first.asScope composePrism some)",
    ScopeTests(
      tuple2Field1[Option[Int], String].first.asScope
        composePrism some[Int]
    )
  )
  checkAll("(tuple2Field2.second.asScope composePrism some)",
    ScopeTests(
      tuple2Field2[Int, Option[String]].second.asScope
        composePrism some[String]
    )
  )

  // Scope composeIso
  checkAll("(tuple2Field1.first.asScope composeIso tuple2Reverse.reverse)",
    ScopeTests(
      tuple2Field1[(Int, Boolean), String].first.asScope
        composeIso tuple2Reverse[Int, Boolean].reverse
    )
  )
  checkAll("(tuple2Field2.second.asScope composeIso tuple2Reverse.reverse)",
    ScopeTests(
      tuple2Field2[Int, (Boolean, String)].second.asScope
        composeIso tuple2Reverse[Boolean, String].reverse
    )
  )
}
