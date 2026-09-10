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

package net.andimiller.schrodinger.simple

import cats.Eq
import cats.kernel.BoundedSemilattice
import net.andimiller.schrodinger.Cardinality
import net.andimiller.schrodinger.Hasher

import scala.collection.immutable.SortedSet

/** A simple CVM sketch (Chakraborty, Vinodchandran and Meel) intended for use as learning material, it won't perform as well as optimised
  * ones.
  *
  * CVM estimates how many distinct items a stream has seen by keeping a uniform random sample: each hash is kept with probability `p`, and
  * when the sample overflows the capacity we halve `p` and drop the hashes that no longer qualify. The estimate is the sample size divided
  * by `p`.
  *
  * Textbook CVM flips a coin per item, which makes the sketch random and therefore unmergeable. Here the randomness comes from the hash
  * itself: a hash is kept iff it is below a threshold, and because the threshold is a power of two the whole thing is integer arithmetic.
  * The sketch is deterministic given the hasher, so it is a law-abiding `BoundedSemilattice`, and merging two sketches built with the same
  * hasher gives exactly the sketch the concatenated streams would have produced — merges are lossless, not approximate.
  *
  * The invariant is `samples = { h : h < 2^(31 - halvings) }` with `samples.size <= maxItems`, and `estimate = samples.size / 2^-halvings`.
  * When `halvings` is 0 the threshold covers the whole hash range, so the sketch is exact — the same "exact while small" story as
  * `SimpleThetaSketch`.
  *
  * @param samples
  *   sampled hashes, masked into `[0, 2^31)`
  * @param halvings
  *   how many times the inclusion probability has been halved; the probability is `2^-halvings`
  * @tparam LgK
  *   log of the number of items to keep
  */
case class SimpleCvmSketch[LgK <: Int: ValueOf](samples: SortedSet[Int], halvings: Int) {
  lazy val maxItems: Int = Math.pow(2, valueOf[LgK].toDouble).toInt

  /** Inclusion probability of each hash, `2^-halvings`. */
  lazy val probability: Double = Math.pow(2, -halvings.toDouble)

  lazy val estimate: Double = samples.size.toDouble / probability

  /** Add a hash, keeping it only if it is below the current threshold, halving the threshold on overflow. */
  def add(hash: Int): SimpleCvmSketch[LgK] = {
    val h = hash & Int.MaxValue
    if (h.toLong < threshold(halvings)) {
      val (s, k) = shrink(samples.incl(h), halvings)
      SimpleCvmSketch(s, k)
    } else this
  }

  private def threshold(k: Int): Long =
    if (k >= 32) 0L else 1L << (31 - k)

  /** Drop hashes above the threshold, then halve the threshold until the sample fits within capacity. */
  private def shrink(initial: SortedSet[Int], initialHalvings: Int): (SortedSet[Int], Int) = {
    var s      = initial.filter(_.toLong < threshold(initialHalvings))
    var halved = initialHalvings
    while (s.size > maxItems) {
      halved += 1
      s = s.filter(_.toLong < threshold(halved))
    }
    (s, halved)
  }
}

object SimpleCvmSketch {

  def empty[LgK <: Int: ValueOf]: SimpleCvmSketch[LgK] =
    SimpleCvmSketch[LgK](SortedSet.empty, 0)

  def fromItems[LgK <: Int: ValueOf, Input](
      items: LazyList[Input]
  )(implicit hasher: Hasher[Input, Int]): SimpleCvmSketch[LgK] =
    items.foldLeft(empty[LgK])((sketch, item) => sketch.add(hasher.hash(item)))

  implicit def boundedSemilattice[LgK <: Int: ValueOf]: BoundedSemilattice[SimpleCvmSketch[LgK]] =
    new BoundedSemilattice[SimpleCvmSketch[LgK]] {

      override def empty: SimpleCvmSketch[LgK] = SimpleCvmSketch.empty[LgK]

      override def combine(
          x: SimpleCvmSketch[LgK],
          y: SimpleCvmSketch[LgK]
      ): SimpleCvmSketch[LgK] = {
        // the finer threshold wins: a sketch that halved more overflowed at every coarser
        // threshold, so the merge can never keep fewer halvings than either input; shrink
        // then drops the coarser sketch's hashes above the threshold
        val (s, k) = x.shrink(x.samples ++ y.samples, math.max(x.halvings, y.halvings))
        SimpleCvmSketch(s, k)
      }
    }

  implicit def eq[LgK <: Int]: Eq[SimpleCvmSketch[LgK]] =
    Eq.by(s => (s.samples.toSet, s.halvings))

  implicit def cardinality[LgK <: Int]: Cardinality[SimpleCvmSketch[LgK]] =
    _.estimate

}
