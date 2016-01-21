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

import monocle.law.discipline.{OptionalTests, SetterTests}
import org.scalacheck.Arbitrary, Arbitrary._
import scalaz._, Scalaz._

import au.com.cba.omnia.spectroscopy.law.discipline.EScopeTests

class EScopeSpec extends SpectroscopySuite {
  // Iso asScope
  checkAll("tuple2Reverse.reverse.asEScope",
    EScopeTests(tuple2Reverse[Int, String].reverse.asEScope))

  // Lens asEScope
  checkAll("tuple2Field1.first.asEScope",
    EScopeTests(tuple2Field1[Int, String].first.asEScope)
  )
  checkAll("tuple2Field2.second.asEScope",
    EScopeTests(tuple2Field2[Int, String].second.asEScope)
  )

  // Prism asEScope
  checkAll("some.asEScope",
    EScopeTests(some[Int].asEScope)
  )
  checkAll("none.asEScope",
    EScopeTests(none[Int].asEScope)
  )
  checkAll("stdLeft.asEScope",
    EScopeTests(stdLeft[Int, String].asEScope)
  )
  checkAll("stdRight.asEScope",
    EScopeTests(stdRight[Int, String].asEScope)
  )

  // Lens composeEScope
  checkAll("(tuple2Field1.first composeEScope some.asEScope)",
    EScopeTests(
      tuple2Field1[Option[Int], String].first
        composeEScope some[Int].asEScope
    )
  )
  checkAll("(tuple2Field2.second composeEScope some.asEScope)",
    EScopeTests(
      tuple2Field2[Int, Option[String]].second
        composeEScope some[String].asEScope
    )
  )

  // EScope composePrism
  checkAll("(tuple2Field1.first.asEScope composePrism some)",
    EScopeTests(
      tuple2Field1[Option[Int], String].first.asEScope
        composePrism some[Int]
    )
  )
  checkAll("(tuple2Field2.second.asEScope composePrism some)",
    EScopeTests(
      tuple2Field2[Int, Option[String]].second.asEScope
        composePrism some[String]
    )
  )

  // EScope composeIso
  checkAll("(tuple2Field1.first.asEScope composeIso tuple2Reverse.reverse)",
    EScopeTests(
      tuple2Field1[(Int, Boolean), String].first.asEScope
        composeIso tuple2Reverse[Int, Boolean].reverse
    )
  )
  checkAll("(tuple2Field2.second.asEScope composeIso tuple2Reverse.reverse)",
    EScopeTests(
      tuple2Field2[Int, (Boolean, String)].second.asEScope
        composeIso tuple2Reverse[Boolean, String].reverse
    )
  )

  // EScope asOptional
  checkAll("tuple2Reverse.reverse.asEScope.asOptional",
    OptionalTests(tuple2Reverse[Int, String].reverse.asEScope.asOptional)
  )
  checkAll("tuple2Field1.first.asEScope.asOptional",
    OptionalTests(tuple2Field1[Int, String].first.asEScope.asOptional)
  )
  checkAll("tuple2Field2.second.asEScope.asOptional",
    OptionalTests(tuple2Field2[Int, String].second.asEScope.asOptional)
  )
  checkAll("some.asEScope.asOptional",
    OptionalTests(some[Int].asEScope.asOptional)
  )
  checkAll("stdLeft.asEScope.asOptional",
    OptionalTests(stdLeft[Int, String].asEScope.asOptional)
  )
  checkAll("(tuple2Field1.first.asEScope composeIso tuple2Reverse.reverse asOptional)",
    OptionalTests(
      tuple2Field1[(Int, Boolean), String].first.asEScope
        composeIso tuple2Reverse[Int, Boolean].reverse
        asOptional
    )
  )
  checkAll("(tuple2Field2.second.asEScope composeIso tuple2Reverse.reverse asOptional)",
    OptionalTests(
      tuple2Field2[Int, (Boolean, String)].second.asEScope
        composeIso tuple2Reverse[Boolean, String].reverse
        asOptional
    )
  )

  // EScope asSetter
  checkAll("tuple2Reverse.reverse.asEScope.asSetter",
    SetterTests(tuple2Reverse[Int, String].reverse.asEScope.asSetter)
  )
  checkAll("tuple2Field1.first.asEScope.asSetter",
    SetterTests(tuple2Field1[Int, String].first.asEScope.asSetter)
  )
  checkAll("tuple2Field2.second.asEScope.asSetter",
    SetterTests(tuple2Field2[Int, String].second.asEScope.asSetter)
  )
  checkAll("some.asEScope.asSetter",
    SetterTests(some[Int].asEScope.asSetter)
  )
  checkAll("stdLeft.asEScope.asSetter",
    SetterTests(stdLeft[Int, String].asEScope.asSetter)
  )
  checkAll("(tuple2Field1.first.asEScope composeIso tuple2Reverse.reverse asSetter)",
    SetterTests(
      tuple2Field1[(Int, Boolean), String].first.asEScope
        composeIso tuple2Reverse[Int, Boolean].reverse
        asSetter
    )
  )
  checkAll("(tuple2Field2.second.asEScope composeIso tuple2Reverse.reverse asSetter)",
    SetterTests(
      tuple2Field2[Int, (Boolean, String)].second.asEScope
        composeIso tuple2Reverse[Boolean, String].reverse
        asSetter
    )
  )

  // EScope first
  checkAll("tuple2Reverse.reverse.asEScope.first",
    EScopeTests(tuple2Reverse[Int, String].reverse.asEScope.first[Char])
  )
  checkAll("tuple2Field1.first.asEScope.first",
    EScopeTests(tuple2Field1[Int, String].first.asEScope.first[Char])
  )
  checkAll("tuple2Field2.second.asEScope.first",
    EScopeTests(tuple2Field2[Int, String].second.asEScope.first[Char])
  )
  checkAll("some.asEScope.first",
    EScopeTests(some[Int].asEScope.first[Char])
  )
  checkAll("stdLeft.asEScope.first",
    EScopeTests(stdLeft[Int, String].asEScope.first[Char])
  )
  checkAll("(tuple2Field1.first.asEScope composeIso tuple2Reverse.reverse first)",
    EScopeTests(
      (
        tuple2Field1[(Int, Boolean), String].first.asEScope
          composeIso tuple2Reverse[Int, Boolean].reverse
      ).first[Char]
    )
  )
  checkAll("(tuple2Field2.second.asEScope composeIso tuple2Reverse.reverse first)",
    EScopeTests(
      (
        tuple2Field2[Int, (Boolean, String)].second.asEScope
          composeIso tuple2Reverse[Boolean, String].reverse
      ).first[Char]
    )
  )

  // EScope second[Char]
  checkAll("tuple2Reverse.reverse.asEScope.second",
    EScopeTests(tuple2Reverse[Int, String].reverse.asEScope.second[Char])
  )
  checkAll("tuple2Field1.first.asEScope.second",
    EScopeTests(tuple2Field1[Int, String].first.asEScope.second[Char])
  )
  checkAll("tuple2Field2.second.asEScope.second",
    EScopeTests(tuple2Field2[Int, String].second.asEScope.second[Char])
  )
  checkAll("some.asEScope.second",
    EScopeTests(some[Int].asEScope.second[Char])
  )
  checkAll("stdLeft.asEScope.second",
    EScopeTests(stdLeft[Int, String].asEScope.second[Char])
  )
  checkAll("(tuple2Field1.first.asEScope composeIso tuple2Reverse.reverse second)",
    EScopeTests(
      (
        tuple2Field1[(Int, Boolean), String].first.asEScope
          composeIso tuple2Reverse[Int, Boolean].reverse
      ).second[Char]
    )
  )
  checkAll("(tuple2Field2.second.asEScope composeIso tuple2Reverse.reverse second)",
    EScopeTests(
      (
        tuple2Field2[Int, (Boolean, String)].second.asEScope
          composeIso tuple2Reverse[Boolean, String].reverse
      ).second[Char]
    )
  )
}
