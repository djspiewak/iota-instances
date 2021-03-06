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

import scalaz.Equal
import TListK.:::
import iotaz.CopK

import scala.language.higherKinds

sealed trait EqualKHelper[LL <: TListK, A] {
  def materialize(offset: Int): Equal[CopK[LL, A]]
}

object EqualKHelper {

  implicit def base[A]: EqualKHelper[TNilK, A] = new EqualKHelper[TNilK, A] {
    def materialize(offset: Int): Equal[CopK[TNilK, A]] = {
      Equal equal { (_, _) => false }
    }
  }

  implicit def induct[F[_], A, LL <: TListK](implicit eql: Equal[F[A]], LL: EqualKHelper[LL, A]): EqualKHelper[F ::: LL, A] = new EqualKHelper[F ::: LL, A] {
    type CP[B] = CopK[F ::: LL, B]

    def materialize(offset: Int): Equal[CP[A]] = {
      val FA = mkInject[F, F ::: LL](offset)

      Equal equal {
        case (FA(left), FA(right)) => eql.equal(left, right)
        case (left, right) => LL.materialize(offset + 1).equal(left.asInstanceOf[CopK[LL, A]], right.asInstanceOf[CopK[LL, A]])
      }
    }
  }

}
