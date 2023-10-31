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

/** Wraps the hash4j MinHash to provide a user friendly interface
  *
  * Only a bit width of 64 is supported, due to a bug in hash4j
  * @param value
  * @tparam Components
  *   number of component hashes inside the MinHash
  */
case class MinHash[Components <: Int: ValueOf](
    value: Array[Byte]
) {
  private val hashing: SimilarityHashPolicy       =
    SimilarityHashing.minHash(valueOf[Components], 64)
  def jaccard(other: MinHash[Components]): Double =
    hashing.getFractionOfEqualComponents(value, other.value)
}

object MinHash {

  def fromItems[Components <: Int: ValueOf, Input](
      items: NonEmptyLazyList[Input],
      chunkSize: Int = 1024
  )(implicit hasher: Hasher[Input, Long]): MinHash[Components] = {
    val hashing   = SimilarityHashing.minHash(valueOf[Components], 64)
    val minHasher = hashing.createHasher()
    NonEmptyLazyList
      .fromLazyListUnsafe(
        LazyList.from(
          items.toLazyList
            .map(hasher.hash)
            .grouped(chunkSize)
            .map { chunk =>
              MinHash[Components](
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

  implicit def hash4jMinHashInstance[
      Components <: Int: ValueOf
  ]: SimilarityHash[MinHash[Components]] =
    new SimilarityHash[MinHash[Components]] {
      override def fromHashes(
          hashes: NonEmptyLazyList[Long]
      ): MinHash[Components] = {
        implicit val hasher: Hasher[Long, Long] = identity
        MinHash.fromItems[Components, Long](hashes)
      }

      override def combine(
          x: MinHash[Components],
          y: MinHash[Components]
      ): MinHash[Components] = {
        MinHash(
          GenericMinHashMerge.merge(
            valueOf[Components],
            64,
            x.value,
            y.value
          )
        )
      }
    }

  implicit def hash4jMinHashEquals[Components <: Int]: Eq[MinHash[Components]] =
    (x: MinHash[Components], y: MinHash[Components]) => x.value.sameElements(y.value)
}
