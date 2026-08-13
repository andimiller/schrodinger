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
import cats.kernel.laws.BoundedSemilatticeLaws
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import munit.DisciplineSuite
import net.andimiller.schrodinger.{HasherFactory, QuantumBoolean}
import net.andimiller.schrodinger.simple.arb.SimpleBloomFilterArbitraries

class SimpleBloomFilterTests
    extends DisciplineSuite
    with BoundedSemilatticeTests[SimpleBloomFilter[256, String]]
    with SimpleBloomFilterArbitraries {

  implicit val hasherFactory: HasherFactory[Int, String, Int] =
    HasherFactory.murmur3

  checkAll(
    "SimpleBloomFilter[256, String]",
    boundedSemilattice
  )

  test("bloom filter should work with simple stuff") {
    val bloom = SimpleBloomFilter
      .empty[256, String]
      .add("hello")
      .add("world")

    assertEquals(
      bloom.contains("hello"),
      QuantumBoolean.Maybe
    )

    assertEquals(
      bloom.contains("goodbye"),
      QuantumBoolean.False
    )
  }

  test("the backing set should stay bounded by Bits") {
    val bloom = SimpleBloomFilter
      .empty[256, String]
      .add("hello")
      .add("world")

    assert(bloom.set.size <= 256)
    assert(bloom.set.max < 256)
  }

  test("merging filters should answer Maybe for items from either") {
    val one    = SimpleBloomFilter.empty[256, String].add("hello")
    val two    = SimpleBloomFilter.empty[256, String].add("world")
    val merged = one |+| two

    assertEquals(merged.contains("hello"), QuantumBoolean.Maybe)
    assertEquals(merged.contains("world"), QuantumBoolean.Maybe)
  }

  override def laws: BoundedSemilatticeLaws[SimpleBloomFilter[256, String]] =
    BoundedSemilatticeLaws[SimpleBloomFilter[256, String]]
}
