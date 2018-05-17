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

package iotaz
package instances

import org.specs2.mutable._
import scalaz._, Scalaz._
import scalaz.syntax.equal
import TListK.:::

object EqualSpecs extends Specification {

  "equal materialization" should {
    type CP[A] = CopK[List ::: Option ::: TNilK, A]

    val LI = CopK.Inject[List, CP]
    val OI = CopK.Inject[Option, CP]

    "materialize first component match true" in {
      val test1 = LI(List(1, 2, 3))

      (test1 === test1) must beTrue
    }

    "materialize first component match false" in {
      val test1 = LI(List(1, 2, 3))
      val test2 = LI(List(3, 4, 5))

      (test1 === test2) must beFalse
    }

    "materialize second component match true" in {
      val test1 = OI(Some("forty-two"))

      (test1 === test1) must beTrue
    }

    "materialize second component match false" in {
      val test1 = OI(Some("forty-two"))
      val test2 = OI(Some("other-things"))

      (test1 === test2) must beFalse
    }

    "materialize second component match false" in {
      val test1 = OI(Some("forty-two"))
      val test2 = OI(Some("other-things"))

      (test1 === test2) must beFalse
    }

    /*"materialize both components match false" in {
      val test1 = LI(List(1, 2, 3, 4))
      val test2 = OI(Some("other-things"))

      (test1 === test2) must beFalse
    }*/
  }
}
