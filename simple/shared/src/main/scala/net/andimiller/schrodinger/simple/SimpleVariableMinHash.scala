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
import net.andimiller.schrodinger.HashTruncator
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.SimilarityHash
import scodec.bits.BitVector

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/** An implementation of MinHash using truncated hashes stored in a Vector
  *
  * This exists as an example of how to implement MinHash, written in the
  * clearest way possible to demonstrate the algorithm, this may lead to bad
  * performance.
  *
  * @param hashes
  *   the hashes stored
  * @tparam HashCount
  *   Number of hash variants to store, this should be the length of the Vector
  * @tparam HashWidth
  *   Width of hashes to store
  */
case class SimpleVariableMinHash[HashCount <: Int, HashWidth <: Int](
    hashes: Vector[BitVector]
) {
  def serialize: BitVector = hashes.reduce(_ ++ _)
}

object SimpleVariableMinHash {

  def fromItems[
      HashCount <: Int: ValueOf,
      HashWidth <: Int: ValueOf,
      Input,
      Hash
  ](
      items: NonEmptyLazyList[Input]
  )(implicit
      hasherFactory: HasherFactory[Int, Input, Hash],
      truncator: HashTruncator[Hash, HashWidth]
  ): SimpleVariableMinHash[HashCount, HashWidth] = {
    require(
      valueOf[HashWidth] <= 32,
      "HashWidth must be 32 or less when used with a 32-bit hasher"
    )
    val hashers = (0 until valueOf[HashCount]).toVector
      .map(hasherFactory.create)

    val empty = mutable.IndexedBuffer.fill(valueOf[HashCount])(
      BitVector.high(valueOf[HashWidth].toLong)
    )

    SimpleVariableMinHash[HashCount, HashWidth](
      items
        .foldLeft(empty) { case (output, item) =>
          output.indices.toVector.foreach { idx =>
            val truncated = truncator.run(hashers(idx).hash(item))
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
      left: SimpleVariableMinHash[HashCount, HashWidth],
      right: SimpleVariableMinHash[HashCount, HashWidth]
  ): Double = {
    val i = (left.hashes zip right.hashes).count { case (l, r) => l == r }
    val u = valueOf[HashCount]
    i.toDouble / u.toDouble
  }

  // when combining minhashes, we take the minimum hash for each index
  implicit def instance[HashCount <: Int: ValueOf, HashWidth <: Int: ValueOf](
      implicit t: HashTruncator[Int, HashWidth]
  ): SimilarityHash[SimpleVariableMinHash[HashCount, HashWidth]] =
    new SimilarityHash[SimpleVariableMinHash[HashCount, HashWidth]] {
      override def fromHashes(
          hashes: NonEmptyLazyList[Long]
      ): SimpleVariableMinHash[HashCount, HashWidth] = {
        implicit val hasher: HasherFactory[Int, Long, Int] = seed =>
          long => MurmurHash3.stringHash(long.toString, seed)
        SimpleVariableMinHash.fromItems[HashCount, HashWidth, Long, Int](hashes)
      }

      override def combine(
          x: SimpleVariableMinHash[HashCount, HashWidth],
          y: SimpleVariableMinHash[HashCount, HashWidth]
      ): SimpleVariableMinHash[HashCount, HashWidth] = {
        SimpleVariableMinHash(x.hashes.zip(y.hashes).map { case (l, r) =>
          Ordering[BitVector].min(l, r)
        })
      }
    }

  implicit def eq[HashCount <: Int, HashWidth <: Int]
      : Eq[SimpleVariableMinHash[HashCount, HashWidth]] = Eq.instance {
    (a, b) =>
      a.hashes == b.hashes
  }

}
