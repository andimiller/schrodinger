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
import net.andimiller.schrodinger.simple.arb.SimpleCvmSketchArbitraries

class SimpleCvmSketchTests extends DisciplineSuite with BoundedSemilatticeTests[SimpleCvmSketch[4]] with SimpleCvmSketchArbitraries {

  checkAll(
    "SimpleCvmSketch[4]",
    boundedSemilattice
  )

  test("Cardinality should be exact while the sample fits") {
    implicit val hasher: Hasher[String, Int] =
      HasherFactory.murmur3.create(0)
    val sketch                               = SimpleCvmSketch.fromItems[12, String](
      LazyList.range(0, 1000).map(_.toString)
    )

    assertEquals(sketch.estimate, 1000d)
    assertEquals(sketch.halvings, 0)
  }

  test("Cardinality should give an expected value when in sampled mode") {
    implicit val hasher: Hasher[String, Int] =
      HasherFactory.murmur3.create(0)
    val sketch                               = SimpleCvmSketch.fromItems[9, String](
      LazyList.range(0, 1000).map(_.toString)
    )

    assertEqualsDouble(
      sketch.estimate,
      1000,
      300
    )
  }

  test("merging should be equivalent to processing the concatenated stream") {
    implicit val hasher: Hasher[String, Int] =
      HasherFactory.murmur3.create(0)
    val one                                  = SimpleCvmSketch.fromItems[9, String](
      LazyList.range(0, 1000).map(_.toString)
    )
    val two                                  = SimpleCvmSketch.fromItems[9, String](
      LazyList.range(500, 1500).map(_.toString)
    )
    val merged                               = NonEmptyList.of(one, two).combineAll
    val concatenated                         = SimpleCvmSketch.fromItems[9, String](
      LazyList.range(0, 1500).map(_.toString)
    )

    assertEquals(merged, concatenated)
  }

  override def laws: BoundedSemilatticeLaws[SimpleCvmSketch[4]] =
    BoundedSemilatticeLaws[SimpleCvmSketch[4]]
}
