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

import cats.data.NonEmptyLazyList
import munit.DisciplineSuite
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.HashesArbitrary
import net.andimiller.schrodinger.SimilarityHashLaws
import net.andimiller.schrodinger.SimilarityHashTests
import net.andimiller.schrodinger.simple.arb.SimpleMinHashArbitraries

class SimpleMinHashTests
    extends DisciplineSuite
    with SimilarityHashTests[SimpleMinHash[32]]
    with SimpleMinHashArbitraries
    with HashesArbitrary {

  checkAll(
    "SimpleMinHash[32]",
    similarityHash
  )

  test("Jaccard should give an expected value") {
    implicit val hasherFactory: HasherFactory[Int, String, Int] =
      HasherFactory.murmur3
    val one                                                     =
      SimpleMinHash.fromItems[1024, String](NonEmptyLazyList("hello", "world"))
    val two                                                     =
      SimpleMinHash.fromItems[1024, String](NonEmptyLazyList("hello"))

    assertEqualsDouble(
      SimpleMinHash.jaccard(one, two),
      0.5,
      delta = 0.03,
      "Expected jaccard to be around 0.5"
    )
  }

  override def laws: SimilarityHashLaws[SimpleMinHash[32]] =
    SimilarityHashLaws[SimpleMinHash[32]]
}
