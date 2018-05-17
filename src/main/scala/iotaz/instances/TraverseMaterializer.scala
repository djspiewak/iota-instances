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

import iotaz.TListK.:::
import iotaz.{ CopK, TListK, TNilK }
import scalaz.{ Applicative, Traverse }

import scala.language.higherKinds

sealed trait TraverseMaterializer[LL <: TListK] {
  def materialize(offset: Int): Traverse[CopK[LL, ?]]
}

object TraverseMaterializer {

  implicit val base: TraverseMaterializer[TNilK] = new TraverseMaterializer[TNilK] {
    override def materialize(offset: Int): Traverse[CopK[TNilK, ?]] = {
      ???
    }
  }

  implicit def induct[F[_], LL <: TListK](
    implicit
    F: Traverse[F],
    LL: TraverseMaterializer[LL]
  ): TraverseMaterializer[F ::: LL] = new TraverseMaterializer[F ::: LL] {
    override def materialize(offset: Int): Traverse[CopK[TListK.:::[F, LL], ?]] = {
      val I = mkInject[F, F ::: LL](offset)

      new Traverse[CopK[F ::: LL, ?]] {
        override def traverseImpl[G[_], A, B](cfa: CopK[F ::: LL, A])(f: A => G[B])(implicit A: Applicative[G]): G[CopK[F ::: LL, B]] = {
          cfa match {
            case I(fa) => A.map(F.traverse(fa)(f))(I(_))
            case other => LL.materialize(offset + 1).traverseImpl(other.asInstanceOf[CopK[LL, A]])(f).asInstanceOf[G[iotaz.CopK[F ::: LL,B]]]
          }
        }
      }
    }
  }

}