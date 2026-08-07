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
import cats.implicits.*
import cats.kernel.BoundedSemilattice
import cats.kernel.laws.BoundedSemilatticeLaws
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import munit.DisciplineSuite
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.arb.SimpleSetSketchArbitraries

import java.nio.ByteBuffer

class SimpleSetSketchTests
    extends DisciplineSuite
    with BoundedSemilatticeTests[SimpleSetSketch[8]]
    with SimpleSetSketchArbitraries {

  checkAll(
    "SimpleSetSketch[8]",
    boundedSemilattice
  )

  implicit val hasherFactory: HasherFactory[Int, String, Long] = { seed => str =>
    {
      val upper = HasherFactory.murmur3.create(seed).hash(str)
      val lower = HasherFactory.murmur3.create(seed ^ 0x9e3779b9).hash(str)
      ByteBuffer.allocate(8).putInt(upper).putInt(lower).getLong(0)
    }
  }

  test("A sketch with the wrong number of registers should be rejected") {
    intercept[IllegalArgumentException] {
      SimpleSetSketch[4](Vector(1, 2, 3))
    }
  }

  test("Cardinality of an empty sketch should be 0") {
    assertEquals(BoundedSemilattice[SimpleSetSketch[4]].empty.cardinality, 0.0)
  }

  test("Cardinality should give an expected value for a small set") {
    val sketch = SimpleSetSketch.fromItems[10, String](
      NonEmptyLazyList.fromSeq(LazyList.range(0, 100).map(_.toString)).get
    )

    assertEqualsDouble(
      sketch.cardinality,
      100,
      delta = 15,
      "Expected cardinality to be around 100"
    )
  }

  test("Cardinality should give an expected value for a large set") {
    val sketch = SimpleSetSketch.fromItems[10, String](
      NonEmptyLazyList.fromSeq(LazyList.range(0, 10000).map(_.toString)).get
    )

    assertEqualsDouble(
      sketch.cardinality,
      10000,
      delta = 1000,
      "Expected cardinality to be around 10000"
    )
  }

  test("Cardinality of merged disjoint sets should give an expected value") {
    val one = SimpleSetSketch.fromItems[10, String](
      NonEmptyLazyList.fromSeq(LazyList.range(0, 500).map(_.toString)).get
    )
    val two = SimpleSetSketch.fromItems[10, String](
      NonEmptyLazyList.fromSeq(LazyList.range(500, 1500).map(_.toString)).get
    )

    val result = List(one, two).combineAll.cardinality

    assertEqualsDouble(
      result,
      1500,
      delta = 150,
      "Expected cardinality to be around 1500"
    )
  }

  test("Jaccard of a sketch with itself should be 1.0") {
    val sketch = SimpleSetSketch.fromItems[10, String](
      NonEmptyLazyList.fromSeq(LazyList.range(0, 1000).map(_.toString)).get
    )

    assertEquals(SimpleSetSketch.jaccard(sketch, sketch), 1.0)
  }

  test("Jaccard should give an expected value for overlapping sets") {
    val one = SimpleSetSketch.fromItems[10, String](
      NonEmptyLazyList.fromSeq(LazyList.range(0, 1000).map(_.toString)).get
    )
    val two = SimpleSetSketch.fromItems[10, String](
      NonEmptyLazyList.fromSeq(LazyList.range(500, 1500).map(_.toString)).get
    )

    // overlap of 500, union of 1500 -> jaccard of 1/3
    assertEqualsDouble(
      SimpleSetSketch.jaccard(one, two),
      1.0 / 3.0,
      delta = 0.1,
      "Expected jaccard to be around 1/3"
    )
  }

  override def laws: BoundedSemilatticeLaws[SimpleSetSketch[8]] =
    BoundedSemilatticeLaws[SimpleSetSketch[8]]
}
