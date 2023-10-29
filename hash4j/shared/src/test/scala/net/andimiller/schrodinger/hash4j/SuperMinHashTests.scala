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

import cats.data.NonEmptyLazyList
import com.dynatrace.hash4j.hashing.Hashing
import munit.DisciplineSuite
import net.andimiller.schrodinger.Hasher
import net.andimiller.schrodinger.HashesArbitrary
import net.andimiller.schrodinger.SimilarityHashLaws
import net.andimiller.schrodinger.SimilarityHashTests
import net.andimiller.schrodinger.hash4j.arb.SuperMinHashArbitraries

class SuperMinHashTests
    extends DisciplineSuite
    with SimilarityHashTests[SuperMinHash[512, 16]]
    with SuperMinHashArbitraries
    with HashesArbitrary {

  checkAll(
    "SuperMinHash[512, 16]",
    semilattice
  )

  test("Should do some vaguely sensible jaccard calculations") {
    implicit val wyhash: Hasher[String, Long] =
      Hashing.wyhashFinal4().hashCharsToLong(_)
    val one =
      SuperMinHash.fromItems[512, 16, String](
        NonEmptyLazyList("hello", "world")
      )
    val two =
      SuperMinHash.fromItems[512, 16, String](NonEmptyLazyList("hello"))

    assertEqualsDouble(
      one jaccard two,
      0.5,
      0.01,
      "jaccard should be around 0.5"
    )
  }

  override def laws: SimilarityHashLaws[SuperMinHash[512, 16]] =
    SimilarityHashLaws[SuperMinHash[512, 16]]
}
