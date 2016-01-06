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

import monocle.function.GenericOptics
import monocle.generic.GenericInstances
import monocle.refined.RefinedInstances
import monocle.state.StateLensSyntax
import monocle.std.StdInstances
import monocle.syntax.Syntaxes
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

trait SpectroscopySuite extends FunSuite
                        with Discipline
                        with Matchers
                        with StdInstances
                        with GenericOptics
                        with GenericInstances
                        with RefinedInstances
                        with Syntaxes
                        with StateLensSyntax
