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

import cats.kernel.Semilattice

/** A type class for sketches that can estimate the Jaccard similarity of two sets. */
trait Jaccard[T] {
  def jaccard(left: T, right: T): Double
}

object Jaccard {

  def apply[T](implicit jaccard: Jaccard[T]): Jaccard[T] =
    jaccard

  /** Derive a Jaccard from inclusion-exclusion: J = (|U| + |V| − |U∪V|) / |U∪V|.
    *
    * This works for any sketch that can estimate its own cardinality and whose `combine` is the
    * union — which is every cardinality sketch in this library. It is the "naive" estimator from
    * the SetSketch paper: it only uses the three cardinalities, discarding any per-component joint
    * information the sketch may hold. Summon it explicitly when needed; it is not an implicit
    * derivation, so it can never silently override a sketch's own (better) Jaccard instance.
    */
  def fromCardinalityAndSemilattice[T](implicit
      cardinality: Cardinality[T],
      semilattice: Semilattice[T]
  ): Jaccard[T] =
    new Jaccard[T] {
      override def jaccard(left: T, right: T): Double = {
        val unionCardinality = cardinality.estimate(semilattice.combine(left, right))
        if (unionCardinality == 0.0) {
          0.0 // both sets are empty, so Jaccard is 0/0; conventionally 0
        } else {
          val estimate =
            (cardinality.estimate(left) + cardinality.estimate(right) - unionCardinality) / unionCardinality
          Math.max(0.0, Math.min(1.0, estimate)) // estimation noise can stray outside [0, 1]
        }
      }
    }

}
