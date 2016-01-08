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

import monocle.law.discipline.PrismTests
import org.scalacheck.{Arbitrary, Gen}, Arbitrary._
import scalaz._, Scalaz._

import au.com.cba.omnia.spectroscopy.law.discipline.EPrismTests

class EPrismSpec extends SpectroscopySuite {
  // Iso asEPrism
  checkAll("tuple2Reverse.reverse.asEPrism",
    EPrismTests(tuple2Reverse[Int, String].reverse.asEPrism))

  // Prism asEPrism
  checkAll("some.asEPrism",     EPrismTests(some[Int].asEPrism))
  checkAll("none.asEPrism",     EPrismTests(none[Int].asEPrism))
  checkAll("stdLeft.asEPrism",  EPrismTests(stdLeft[Int, String].asEPrism))
  checkAll("stdRight.asEPrism", EPrismTests(stdRight[Int, String].asEPrism))

  // Prism asEPrism asPrism
  checkAll("some.asEPrism.asPrism",     PrismTests(some[Int].asEPrism.asPrism))
  checkAll("none.asEPrism.asPrism",     PrismTests(none[Int].asEPrism.asPrism))
  checkAll("stdLeft.asEPrism.asPrism",  PrismTests(
    stdLeft[Int, String].asEPrism.asPrism
  ))
  checkAll("stdRight.asEPrism.asPrism", PrismTests(
    stdRight[Int, String].asEPrism.asPrism
  ))

  // Example EPrism
  abstract sealed class MultiValue
  case class IntValue(intValue: Int) extends MultiValue
  case class DoubleValue(doubleValue: Double) extends MultiValue
  case class StringValue(stringValue: String) extends MultiValue

  val _IntValue = EPrism[String, MultiValue, Int]({
    case IntValue(intValue) => \/-(intValue)
    case _: DoubleValue     => -\/("Expected IntValue, found DoubleValue")
    case _: StringValue     => -\/("Expected IntValue, found StringValue")
  })(
    IntValue(_)
  )

  implicit val ArbIntValue = Arbitrary(
    for {
      intValue <- arbitrary[Int]
    } yield IntValue(intValue)
  )

  implicit val ArbDoubleValue = Arbitrary(
    for {
      doubleValue <- arbitrary[Double]
    } yield DoubleValue(doubleValue)
  )

  implicit val ArbStringValue = Arbitrary(
    for {
      stringValue <- arbitrary[String]
    } yield StringValue(stringValue)
  )

  implicit val ArbMultiValue = Arbitrary(
    Gen.oneOf(
      arbitrary[IntValue],
      arbitrary[DoubleValue],
      arbitrary[StringValue]
    ).map(identity[MultiValue])
  )

  implicit val EqualMultiValue = Equal.equalA[MultiValue]

  checkAll("_IntValue", EPrismTests(_IntValue))
  checkAll("_IntValue.asPrism", PrismTests(_IntValue.asPrism))

  // EPPrism composeEPrism
  checkAll("some.asEPrism composeEPrism _IntValue", EPrismTests(
    some[MultiValue].asEPrism composeEPrism _IntValue
  ))
  checkAll("stdLeft.asEPrism composeEPrism _IntValue", EPrismTests(
    stdLeft[MultiValue, Int].asEPrism composeEPrism _IntValue
  ))

  // EPPrism composeEPrism asPrism
  checkAll("some.asEPrism composeEPrism _IntValue", PrismTests(
    (some[MultiValue].asEPrism composeEPrism _IntValue).asPrism
  ))
  checkAll("stdLeft.asEPrism composeEPrism _IntValue", PrismTests(
    (stdLeft[MultiValue, Int].asEPrism composeEPrism _IntValue).asPrism
  ))
}
