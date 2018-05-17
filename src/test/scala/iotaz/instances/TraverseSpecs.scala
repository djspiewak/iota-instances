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

package iotaz.instances

import iotaz.{ CopK, TNilK }
import iotaz.TListK.:::
import org.specs2.mutable.Specification
import scalaz._, Scalaz._

object TraverseSpecs extends Specification {

  "traverse materialization" should {
    type CP[A] = CopK[List ::: Option ::: Vector ::: TNilK, A]
    val LI = CopK.Inject[List, CP]
    val OI = CopK.Inject[Option, CP]
    val VI = CopK.Inject[Vector, CP]

    "materialize first element" in {
      val test = LI(List(1, 2, 3))

      val traversed = test.traverse(x => (x + 1).toString.right[String])

      traversed must beEqualTo(LI(List("2", "3", "4")).right[String])
    }

    "materialize second element" in {
      val test = OI(":)".some)

      val traversed = test.traverse(s => List(s, s))

      traversed must beEqualTo(List(OI(":)".some), OI(":)".some)))
    }

    "materialize last element" in {
      val test = VI(Vector(":)", ":D", ":>"))

      val traversed = test.traverse(x => s"[$x]".some)

      traversed must beEqualTo(VI(Vector("[:)]", "[:D]", "[:>]")).some)
    }
  }

}
