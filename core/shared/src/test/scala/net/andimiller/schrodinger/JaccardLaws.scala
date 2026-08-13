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

trait JaccardLaws[T] {
  implicit def J: Jaccard[T]

  def jaccardSelfSimilarity(a: T): Boolean =
    J.jaccard(a, a) == 1.0

  def jaccardSymmetry(a: T, b: T): Boolean =
    J.jaccard(a, b) == J.jaccard(b, a)
}

object JaccardLaws {
  def apply[T](implicit ev: Jaccard[T]): JaccardLaws[T] =
    new JaccardLaws[T] { def J: Jaccard[T] = ev }
}
