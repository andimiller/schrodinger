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
import com.dynatrace.hash4j.similarity.ElementHashProvider
import com.dynatrace.hash4j.similarity.SimilarityHashPolicy
import com.dynatrace.hash4j.similarity.SimilarityHashing
import net.andimiller.schrodinger.Hasher
import net.andimiller.schrodinger.SimilarityHash
import net.andimiller.schrodinger.hash4j.utils.GenericMinHashMerge

/** Wraps the hash4j SuperMinHash to provide a user friendly interface
  * @param value
  * @tparam Components
  *   number of component hashes inside the SuperMinHash
  * @tparam Bits
  *   width of the hashes inside the Minhash
  */
case class SuperMinHash[Components <: Int: ValueOf, Bits <: Int: ValueOf](
    value: Array[Byte]
) {
  private val hashing: SimilarityHashPolicy =
    SimilarityHashing.superMinHash(valueOf[Components], valueOf[Bits])

  def jaccard(other: SuperMinHash[Components, Bits]): Double =
    hashing.getFractionOfEqualComponents(value, other.value)
}

object SuperMinHash {

  def fromItems[Components <: Int: ValueOf, Bits <: Int: ValueOf, Input](
      items: NonEmptyLazyList[Input],
      chunkSize: Int = 1024
  )(implicit hasher: Hasher[Input, Long]): SuperMinHash[Components, Bits] = {
    val hashing =
      SimilarityHashing.superMinHash(valueOf[Components], valueOf[Bits])
    val minHasher = hashing.createHasher()
    NonEmptyLazyList
      .fromLazyListUnsafe(
        LazyList.from(
          items.toLazyList
            .map(hasher.hash)
            .grouped(chunkSize)
            .map { chunk =>
              SuperMinHash[Components, Bits](
                minHasher.compute(
                  ElementHashProvider.ofValues(
                    chunk: _*
                  )
                )
              )
            }
        )
      )
      .reduce

  }

  implicit def hash4jSuperMinHashInstance[
      Components <: Int: ValueOf,
      Bits <: Int: ValueOf
  ]: SimilarityHash[SuperMinHash[Components, Bits]] =
    new SimilarityHash[SuperMinHash[Components, Bits]] {
      override def fromHashes(
          hashes: NonEmptyLazyList[Long]
      ): SuperMinHash[Components, Bits] = {
        implicit val hasher: Hasher[Long, Long] = identity
        fromItems[Components, Bits, Long](hashes)
      }

      override def combine(
          x: SuperMinHash[Components, Bits],
          y: SuperMinHash[Components, Bits]
      ): SuperMinHash[Components, Bits] =
        SuperMinHash(
          GenericMinHashMerge.merge(
            valueOf[Components],
            valueOf[Bits],
            x.value,
            y.value
          )
        )
    }

  implicit def hash4jSuperMinHashEquals[Components <: Int, Bits <: Int]
      : Eq[SuperMinHash[Components, Bits]] =
    (x: SuperMinHash[Components, Bits], y: SuperMinHash[Components, Bits]) =>
      x.value.sameElements(y.value)
}
