/*
 * Copyright 2018 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

name := "iota-instances"

baseVersion in ThisBuild := "0.1"

organization in ThisBuild := "com.codecommit"
organizationName in ThisBuild := "Daniel Spiewak"

libraryDependencies ++= Seq(
  "io.frees"   %% "iotaz-core"  % "0.3.7",
  "org.specs2" %% "specs2-core" % "4.2.0" % Test)
