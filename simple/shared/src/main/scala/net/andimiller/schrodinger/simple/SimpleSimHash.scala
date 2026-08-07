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
import cats.kernel.CommutativeMonoid
import net.andimiller.schrodinger.HasherFactory

/** A SimHash, a locality-sensitive hash for text similarity (Charikar, 2002).
  *
  * A document is a bag of shingles (typically words). Each shingle votes +1 or −1 on every one of
  * the `Components` hash functions, and the document's signature is the majority vote per
  * component: bit i is set when more shingles voted + than − (ties count as 0). Near-duplicate
  * documents share most shingles, so their signatures agree on most bits; similarity is
  * `1 − hammingDistance / Components`.
  *
  * This is the multiset counterpart to MinHash: minhash answers "do these sets overlap?" while
  * simhash answers "do these texts read alike?", where repetition matters. Like
  * SimpleCountMinSketch, merging is element-wise addition and is therefore not idempotent.
  *
  * @param counts
  *   the running vote tally, one per component
  * @tparam Components
  *   number of hash functions (and signature bits)
  * @tparam Input
  *   type of the shingles
  */
case class SimpleSimHash[Components <: Int: ValueOf, Input](counts: Vector[Int])(implicit
    hasherFactory: HasherFactory[Int, Input, Int]
) {
  require(
    counts.length == valueOf[Components],
    s"SimpleSimHash requires ${valueOf[Components]} components, but got ${counts.length}"
  )

  def add(shingle: Input): SimpleSimHash[Components, Input] = SimpleSimHash(
    counts.zipWithIndex.map { case (count, index) =>
      count + (if (hasherFactory.create(index).hash(shingle) < 0) -1 else 1)
    }
  )

  /** The signature: bit i is set when component i has a positive vote tally. */
  def signature: Vector[Boolean] = counts.map(_ > 0)
}

object SimpleSimHash {

  def empty[Components <: Int: ValueOf, Input](implicit
      hasherFactory: HasherFactory[Int, Input, Int]
  ): SimpleSimHash[Components, Input] =
    SimpleSimHash(Vector.fill(valueOf[Components])(0))

  def fromItems[Components <: Int: ValueOf, Input](
      shingles: LazyList[Input]
  )(implicit hasherFactory: HasherFactory[Int, Input, Int]): SimpleSimHash[Components, Input] =
    shingles.foldLeft(empty[Components, Input])((simhash, shingle) => simhash.add(shingle))

  /** Number of signature bits that differ — small for near-duplicates, around half for unrelated texts. */
  def hammingDistance[Components <: Int, Input](
      left: SimpleSimHash[Components, Input],
      right: SimpleSimHash[Components, Input]
  ): Int =
    left.signature.zip(right.signature).count { case (a, b) => a != b }

  /** Text similarity as `1 − hammingDistance / Components`, so 1.0 is identical and ~0.5 is unrelated. */
  def similarity[Components <: Int: ValueOf, Input](
      left: SimpleSimHash[Components, Input],
      right: SimpleSimHash[Components, Input]
  ): Double =
    1.0 - hammingDistance(left, right).toDouble / valueOf[Components]

  implicit def commutativeMonoid[Components <: Int: ValueOf, Input](implicit
      hasherFactory: HasherFactory[Int, Input, Int]
  ): CommutativeMonoid[SimpleSimHash[Components, Input]] =
    new CommutativeMonoid[SimpleSimHash[Components, Input]] {
      override def empty: SimpleSimHash[Components, Input] =
        SimpleSimHash.empty[Components, Input]

      override def combine(
          x: SimpleSimHash[Components, Input],
          y: SimpleSimHash[Components, Input]
      ): SimpleSimHash[Components, Input] =
        SimpleSimHash(x.counts.zip(y.counts).map { case (a, b) => a + b })
    }

  implicit def eq[Components <: Int, Input]: Eq[SimpleSimHash[Components, Input]] =
    Eq.by(_.counts)
}
