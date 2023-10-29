/*
 * Copyright 2023 andimiller
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

package net.andimiller.schrodinger

import cats.data.NonEmptyLazyList
import cats.kernel.laws.IsEq
import cats.kernel.laws.IsEqArrow
import cats.kernel.laws.SemilatticeLaws

trait SimilarityHashLaws[T] extends SemilatticeLaws[T] {
  implicit def S: SimilarityHash[T]

  def distributive(
      a: NonEmptyLazyList[Long],
      b: NonEmptyLazyList[Long]
  ): IsEq[T] =
    S.fromHashes(a ++ b) <-> S.combine(S.fromHashes(a), S.fromHashes(b))
}

object SimilarityHashLaws {
  def apply[A](implicit ev: SimilarityHash[A]): SimilarityHashLaws[A] =
    new SimilarityHashLaws[A] { def S: SimilarityHash[A] = ev }
}
