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

package net.andimiller.schrodinger.hash4j

import cats.Eq
import cats.data.NonEmptyLazyList
import cats.kernel.Semilattice
import com.dynatrace.hash4j.similarity.ElementHashProvider
import com.dynatrace.hash4j.similarity.SimilarityHashPolicy
import com.dynatrace.hash4j.similarity.SimilarityHashing
import com.dynatrace.hash4j.util.PackedArray
import com.dynatrace.hash4j.util.PackedArray.PackedArrayReadIterator
import net.andimiller.schrodinger.Hasher

/** Wraps the hash4j MinHash to provide a user friendly interface
  * @param value
  * @tparam Components
  *   number of component hashes inside the MinHash
  * @tparam Bits
  *   width of the hashes inside the Minhash
  */
case class MinHash[Components <: Int: ValueOf, Bits <: Int: ValueOf](
    value: Array[Byte]
) {
  private val hashing: SimilarityHashPolicy =
    SimilarityHashing.minHash(valueOf[Components], valueOf[Bits])
  def jaccard(other: MinHash[Components, Bits]): Double =
    hashing.getFractionOfEqualComponents(value, other.value)
}

object MinHash {

  implicit class PackedArrayReadIteratorOps(pari: PackedArrayReadIterator) {
    def asScala: Iterator[Long] = new Iterator[Long] {
      override def hasNext: Boolean = pari.hasNext
      override def next(): Long = pari.next()
    }
  }

  def fromItems[Components <: Int: ValueOf, Bits <: Int: ValueOf, Input](
      items: NonEmptyLazyList[Input],
      chunkSize: Int = 1024
  )(implicit hasher: Hasher[Input, Long]) = {
    val hashing = SimilarityHashing.minHash(valueOf[Components], valueOf[Bits])
    val minHasher = hashing.createHasher()
    items.toLazyList
      .map(hasher.hash)
      .grouped(chunkSize)
      .map { chunk =>
        MinHash[Components, Bits](
          minHasher.compute(
            ElementHashProvider.ofValues(
              chunk: _*
            )
          )
        )
      }
      .reduce(merge[Components, Bits])
  }

  def merge[Components <: Int: ValueOf, Bits <: Int: ValueOf](
      left: MinHash[Components, Bits],
      right: MinHash[Components, Bits]
  ): MinHash[Components, Bits] = {
    val pah = PackedArray.getHandler(valueOf[Bits])
    val output = pah.create(valueOf[Components])
    (
      pah
        .readIterator(left.value, valueOf[Components])
        .asScala
        .zip(pah.readIterator(right.value, valueOf[Components]).asScala)
      )
      .zipWithIndex
      .foreach { case ((l, r), idx) =>
        pah.set(output, idx, Math.min(l, r))
      }
    MinHash[Components, Bits](output)
  }

  implicit def hash4jMinHashSemilattice[
      Components <: Int: ValueOf,
      Bits <: Int: ValueOf
  ]: Semilattice[MinHash[Components, Bits]] =
    (x: MinHash[Components, Bits], y: MinHash[Components, Bits]) => merge(x, y)

  implicit def hash4jMinHashEquals[Components <: Int, Bits <: Int]
      : Eq[MinHash[Components, Bits]] =
    (x: MinHash[Components, Bits], y: MinHash[Components, Bits]) =>
      x.value.sameElements(y.value)
}
