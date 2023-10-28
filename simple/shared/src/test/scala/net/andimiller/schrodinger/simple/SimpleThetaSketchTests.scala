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

import cats.data.NonEmptyList
import cats.implicits.*
import cats.kernel.laws.BoundedSemilatticeLaws
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import munit.DisciplineSuite
import net.andimiller.schrodinger.Hasher
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.arb.SimpleThetaSketchArbitraries

class SimpleThetaSketchTests
    extends DisciplineSuite
    with BoundedSemilatticeTests[SimpleThetaSketch[4]]
    with SimpleThetaSketchArbitraries {

  checkAll(
    "SimpleThetaSketch[4]",
    boundedSemilattice
  )

  test("Cardinality should give an expected value when in exact mode") {
    implicit val hasher: Hasher[String, Int] =
      HasherFactory.murmur3.create(0)
    val one = SimpleThetaSketch.fromItems[12, String](
      LazyList.range(0, 1000).map(_.toString)
    )
    val two = SimpleThetaSketch.fromItems[12, String](
      LazyList.range(500, 1500).map(_.toString)
    )

    val result = NonEmptyList.of(one, two).combineAll.cardinality

    assertEquals(
      result,
      1500d
    )
  }

  test("Cardinality should give an expected value when in sampled mode") {
    implicit val hasher: Hasher[String, Int] =
      HasherFactory.murmur3.create(0)
    val one = SimpleThetaSketch.fromItems[8, String](
      LazyList.range(0, 1000).map(_.toString)
    )
    val two = SimpleThetaSketch.fromItems[8, String](
      LazyList.range(500, 1500).map(_.toString)
    )

    val result = NonEmptyList.of(one, two).combineAll.cardinality

    assertEqualsDouble(
      result,
      1500,
      100
    )
  }

  override def laws: BoundedSemilatticeLaws[SimpleThetaSketch[4]] =
    BoundedSemilatticeLaws[SimpleThetaSketch[4]]
}
