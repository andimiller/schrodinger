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

package net.andimiller.schrodinger.hash4j.arb

import cats.data.NonEmptyLazyList
import cats.data.NonEmptyList
import com.dynatrace.hash4j.hashing.Hashing
import net.andimiller.schrodinger.Hasher
import net.andimiller.schrodinger.hash4j.MinHash
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait MinHashArbitraries {

  implicit val nelString: Arbitrary[NonEmptyList[String]] =
    Arbitrary(
      Gen
        .nonEmptyListOf(Gen.alphaNumStr)
        .map(NonEmptyList.fromListUnsafe)
    )

  implicit def simpleMinHashArb[
      Components <: Int: ValueOf
  ]: Arbitrary[MinHash[Components]] = {
    implicit val wyhash: Hasher[String, Long] =
      Hashing.wyhashFinal4().hashCharsToLong(_)
    Arbitrary(
      Gen
        .nonEmptyListOf(Gen.alphaNumStr)
        .map { strings =>
          MinHash.fromItems[Components, String](
            NonEmptyLazyList.fromLazyListUnsafe(
              LazyList.from(strings)
            ),
            chunkSize = 10
          )
        }
    )
  }

}
