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

organization := "au.com.cba.omnia"
name         := "spectroscopy"
scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Xlint",
  "-deprecation",
  "-feature",
  "-language:_",
  "-unchecked"
)

val monocleVersion = "1.2.0"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-state"   % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-refined" % monocleVersion,
  "org.scalaz"                  %%  "scalaz-core"     % "7.2.0"
)

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-law"     % monocleVersion % "test"
)
