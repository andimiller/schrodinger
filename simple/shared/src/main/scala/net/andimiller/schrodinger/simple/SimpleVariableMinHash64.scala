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
import net.andimiller.schrodinger.HashShifter
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.SimilarityHash
import scodec.Attempt
import scodec.DecodeResult
import scodec.bits.BitVector
import scodec.codecs.long
import scodec.interop.cats.*

import java.nio.ByteBuffer
import scala.collection.mutable

/** An implementation of MinHash using shifted long hashes stored in a Vector
  *
  * When serializing we only serialize the bits that matter, so the serialized
  * form is smaller
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
case class SimpleVariableMinHash64[HashCount <: Int, HashWidth <: Int: ValueOf](
    hashes: Vector[Long]
) {
  def serialize: BitVector =
    hashes
      .map { l =>
        {
          BitVector.fromLong(l, size = valueOf[HashWidth])
        }
      }
      .reduce(_ ++ _)
}

object SimpleVariableMinHash64 {

  def deserialize[HashCount <: Int: ValueOf, HashWidth <: Int: ValueOf](
      b: BitVector
  ): Attempt[DecodeResult[SimpleVariableMinHash64[HashCount, HashWidth]]] = {
    Vector
      .fill(valueOf[HashCount])(long(valueOf[HashWidth]).asDecoder)
      .sequence
      .map(SimpleVariableMinHash64[HashCount, HashWidth](_))
      .decode(b)
  }

  val unorderedLong: Ordering[Long] = (x: Long, y: Long) =>
    java.lang.Long.compareUnsigned(x, y)
  val MAX_UNORDERED_LONG: Long = -1L

  def fromItems[
      HashCount <: Int: ValueOf,
      HashWidth <: Int: ValueOf,
      Input
  ](
      items: NonEmptyLazyList[Input]
  )(implicit
      hasherFactory: HasherFactory[Int, Input, Long],
      truncator: HashShifter[Long, HashWidth]
  ): SimpleVariableMinHash64[HashCount, HashWidth] = {
    val hashers = (0 until valueOf[HashCount]).toVector
      .map(hasherFactory.create)

    val empty =
      mutable.IndexedBuffer.fill(valueOf[HashCount])(MAX_UNORDERED_LONG)

    SimpleVariableMinHash64[HashCount, HashWidth](
      items
        .foldLeft(empty) { case (output, item) =>
          output.indices.toVector.foreach { idx =>
            val truncated = truncator.run(hashers(idx).hash(item))
            output(idx) = unorderedLong.min(truncated, output(idx))
          }
          output
        }
        .toVector
    )
  }

  def jaccard[HashCount <: Int: ValueOf, HashWidth <: Int](
      left: SimpleVariableMinHash64[HashCount, HashWidth],
      right: SimpleVariableMinHash64[HashCount, HashWidth]
  ): Double = {
    val i = (left.hashes zip right.hashes).count { case (l, r) => l == r }
    val u = valueOf[HashCount]
    i.toDouble / u.toDouble
  }

  // when combining minhashes, we take the minimum hash for each index
  implicit def instance[HashCount <: Int: ValueOf, HashWidth <: Int: ValueOf]
      : SimilarityHash[SimpleVariableMinHash64[HashCount, HashWidth]] =
    new SimilarityHash[SimpleVariableMinHash64[HashCount, HashWidth]] {
      override def fromHashes(
          hashes: NonEmptyLazyList[Long]
      ): SimpleVariableMinHash64[HashCount, HashWidth] = {
        implicit val hasher: HasherFactory[Int, Long, Long] = seed =>
          long => {
            val upper = HasherFactory.murmur3.create(seed).hash(long.toString)
            val lower =
              HasherFactory.murmur3.create(0 - seed).hash(long.toString)
            ByteBuffer.allocate(8).putInt(upper).putInt(lower).getLong(0)
          }
        SimpleVariableMinHash64.fromItems[HashCount, HashWidth, Long](
          hashes
        )
      }

      override def combine(
          x: SimpleVariableMinHash64[HashCount, HashWidth],
          y: SimpleVariableMinHash64[HashCount, HashWidth]
      ): SimpleVariableMinHash64[HashCount, HashWidth] = {
        SimpleVariableMinHash64(x.hashes.zip(y.hashes).map { case (l, r) =>
          unorderedLong.min(l, r)
        })
      }
    }

  implicit def eq[HashCount <: Int, HashWidth <: Int]
      : Eq[SimpleVariableMinHash64[HashCount, HashWidth]] = Eq.instance {
    (a, b) =>
      a.hashes == b.hashes
  }

}
