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

import cats.implicits.*
import cats.kernel.laws.CommutativeMonoidLaws
import cats.kernel.laws.discipline.CommutativeMonoidTests
import munit.DisciplineSuite
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.arb.SimpleCountMinSketchArbitraries

class SimpleCountMinSketchTests
    extends DisciplineSuite
    with CommutativeMonoidTests[SimpleCountMinSketch[4, 1024, String]]
    with SimpleCountMinSketchArbitraries {

  implicit val hasherFactory: HasherFactory[Int, String, Int] =
    HasherFactory.murmur3

  checkAll(
    "SimpleCountMinSketch[4, 1024, String]",
    commutativeMonoid
  )

  test("query should never underestimate a frequency") {
    val sketch = SimpleCountMinSketch.fromItems[4, 1024, String](
      LazyList.fill(100)("hello")
    )

    assert(sketch.query("hello") >= 100)
  }

  test("query should give an expected value") {
    val words  = LazyList.range(0, 10).map(i => s"word$i")
    val sketch = SimpleCountMinSketch.fromItems[4, 1024, String](
      LazyList.range(0, 100).map(_ % 10).map(i => s"word$i")
    )

    words.foreach { word =>
      assertEqualsDouble(
        sketch.query(word),
        10,
        delta = 3,
        s"Expected the count of $word to be around 10"
      )
    }
  }

  test("combining sketches should not lose counts") {
    val one      = SimpleCountMinSketch.fromItems[4, 1024, String](LazyList.fill(50)("hello"))
    val two      = SimpleCountMinSketch.fromItems[4, 1024, String](LazyList.fill(30)("hello"))
    val combined = one |+| two

    assert(combined.query("hello") >= 80)
    assert(combined.query("hello") >= one.query("hello"))
    assert(combined.query("hello") >= two.query("hello"))
  }

  test("a sketch with the wrong counter dimensions should be rejected") {
    intercept[IllegalArgumentException] {
      SimpleCountMinSketch[4, 1024, String](Vector(Vector(1, 2), Vector(3, 4)))
    }
  }

  override def laws: CommutativeMonoidLaws[SimpleCountMinSketch[4, 1024, String]] =
    CommutativeMonoidLaws[SimpleCountMinSketch[4, 1024, String]]
}
