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

import scalaz.{ Equal, Traverse }

import scala.language.higherKinds

package object instances {

  private[instances] def mkInject[F[_], LL <: TListK](i: Int): CopK.Inject[F, CopK[LL, ?]] = {
    CopK.Inject.injectFromInjectL[F, LL](
      CopK.InjectL.makeInjectL[F, LL](
        new TListK.Pos[LL, F] { val index: Int = i }
      )
    )
  }

  implicit def equalForCopK[LL <: TListK, A](implicit EH: EqualKHelper[LL, A]): Equal[CopK[LL, A]] =
    EH.materialize(0)

  implicit def traverseForCopK[LL <: TListK](implicit TM: TraverseMaterializer[LL]): Traverse[CopK[LL, ?]] =
    TM.materialize(0)
}
