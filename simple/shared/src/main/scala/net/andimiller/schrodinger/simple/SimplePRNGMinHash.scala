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
import cats.data.NonEmptyLazyList
import cats.implicits.toTraverseOps
import net.andimiller.schrodinger.HashTruncator
import net.andimiller.schrodinger.Hasher
import net.andimiller.schrodinger.SimilarityHash
import scodec.Attempt
import scodec.DecodeResult
import scodec.bits.BitVector
import scodec.codecs.bits
import scodec.interop.cats.*

import scala.collection.mutable
import scala.util.Random

/** An implementation of MinHash using truncated hashes stored in a Vector, using a PRNG to try and gain speed
  *
  * This exists as an example of how to implement MinHash, written in the clearest way possible to demonstrate the algorithm, this may lead
  * to bad performance.
  *
  * @param hashes
  *   the hashes stored
  * @tparam HashCount
  *   Number of hash variants to store, this should be the length of the Vector
  * @tparam HashWidth
  *   Width of hashes to store
  */
case class SimplePRNGMinHash[HashCount <: Int, HashWidth <: Int](
    hashes: Vector[BitVector]
) {
  def serialize: BitVector = hashes.reduce(_ ++ _)
}

object SimplePRNGMinHash {

  def deserialize[HashCount <: Int: ValueOf, HashWidth <: Int: ValueOf](
      b: BitVector
  ): Attempt[DecodeResult[SimplePRNGMinHash[HashCount, HashWidth]]] = {
    Vector
      .fill(valueOf[HashCount])(bits(valueOf[HashWidth].toLong).asDecoder)
      .sequence
      .map(SimplePRNGMinHash[HashCount, HashWidth])
      .decode(b)
  }

  def fromItems[
      HashCount <: Int: ValueOf,
      HashWidth <: Int: ValueOf,
      Input
  ](
      items: NonEmptyLazyList[Input]
  )(implicit
      hasher: Hasher[Input, Long],
      truncator: HashTruncator[Long, HashWidth]
  ): SimplePRNGMinHash[HashCount, HashWidth] = {

    val empty = mutable.ArrayBuffer.fill(valueOf[HashCount])(
      BitVector.high(valueOf[HashWidth].toLong)
    )

    SimplePRNGMinHash[HashCount, HashWidth](
      items
        .foldLeft(empty) { case (output, item) =>
          val hash = hasher.hash(item)
          val prng = new Random(hash)
          output.indices.foreach { idx =>
            val truncated = truncator.run(prng.nextLong())
            if (truncated < output(idx)) {
              output(idx) = truncated
            }
          }
          output
        }
        .toVector
    )
  }

  def jaccard[HashCount <: Int: ValueOf, HashWidth <: Int](
      left: SimplePRNGMinHash[HashCount, HashWidth],
      right: SimplePRNGMinHash[HashCount, HashWidth]
  ): Double = {
    val i = (left.hashes zip right.hashes).count { case (l, r) => l == r }
    val u = valueOf[HashCount]
    i.toDouble / u.toDouble
  }

  // when combining minhashes, we take the minimum hash for each index
  implicit def instance[HashCount <: Int: ValueOf, HashWidth <: Int: ValueOf]: SimilarityHash[SimplePRNGMinHash[HashCount, HashWidth]] =
    new SimilarityHash[SimplePRNGMinHash[HashCount, HashWidth]] {
      override def fromHashes(
          hashes: NonEmptyLazyList[Long]
      ): SimplePRNGMinHash[HashCount, HashWidth] = {
        implicit val hasher: Hasher[Long, Long] = identity
        SimplePRNGMinHash.fromItems[HashCount, HashWidth, Long](hashes)
      }

      override def combine(
          x: SimplePRNGMinHash[HashCount, HashWidth],
          y: SimplePRNGMinHash[HashCount, HashWidth]
      ): SimplePRNGMinHash[HashCount, HashWidth] = {
        SimplePRNGMinHash(x.hashes.zip(y.hashes).map { case (l, r) =>
          Ordering[BitVector].min(l, r)
        })
      }
    }

  implicit def eq[HashCount <: Int, HashWidth <: Int]: Eq[SimplePRNGMinHash[HashCount, HashWidth]] = Eq.instance { (a, b) =>
    a.hashes == b.hashes
  }

}
