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

import cats.kernel.laws.CommutativeMonoidLaws
import cats.kernel.laws.discipline.CommutativeMonoidTests
import munit.DisciplineSuite
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.arb.SimpleSimHashArbitraries

class SimpleSimHashTests
    extends DisciplineSuite
    with CommutativeMonoidTests[SimpleSimHash[128, String]]
    with SimpleSimHashArbitraries {

  implicit val hasherFactory: HasherFactory[Int, String, Int] =
    HasherFactory.murmur3

  checkAll(
    "SimpleSimHash[128, String]",
    commutativeMonoid
  )

  // a document with "the" slightly dominant, so shared words dominate the vote tallies
  // but the changed words still carry enough weight to flip some bits
  val docA: LazyList[String] =
    LazyList.fill(20)("the") ++
      LazyList.range(0, 50).map(i => s"word$i").flatMap(word => LazyList(word, word))

  // docA with "word48" and "word49" replaced by "other1" and "other2"
  val docB: LazyList[String] =
    LazyList.fill(20)("the") ++
      (LazyList.range(0, 48).map(i => s"word$i").flatMap(word => LazyList(word, word)) ++
        LazyList("other1", "other1", "other2", "other2"))

  // a document sharing no words with docA
  val docC: LazyList[String] =
    LazyList.range(100, 150).map(i => s"word$i").flatMap(word => LazyList(word, word)) ++
      LazyList.fill(20)("zzz")

  test("identical documents should have identical signatures") {
    val a = SimpleSimHash.fromItems[128, String](docA)
    val b = SimpleSimHash.fromItems[128, String](docA)

    assertEquals(SimpleSimHash.hammingDistance(a, b), 0)
    assertEquals(SimpleSimHash.similarity(a, b), 1.0)
  }

  test("near-duplicate documents should be closer than unrelated ones") {
    val a  = SimpleSimHash.fromItems[128, String](docA)
    val b  = SimpleSimHash.fromItems[128, String](docB)
    val c  = SimpleSimHash.fromItems[128, String](docC)
    val ab = SimpleSimHash.hammingDistance(a, b)
    val ac = SimpleSimHash.hammingDistance(a, c)

    assert(ab < ac, s"expected near-duplicates to be closer: hamming(a,b)=$ab, hamming(a,c)=$ac")
    // a two-word change flips only a few bits
    assert(ab < 30, s"expected near-duplicates to differ in few bits, got $ab")
    // unrelated documents differ in roughly half their bits
    assert(ac > 44 && ac < 84, s"expected hamming(a,c) to be around 64, got $ac")
  }

  test("a sketch with the wrong number of components should be rejected") {
    intercept[IllegalArgumentException] {
      SimpleSimHash[128, String](Vector(1, 2, 3))
    }
  }

  override def laws: CommutativeMonoidLaws[SimpleSimHash[128, String]] =
    CommutativeMonoidLaws[SimpleSimHash[128, String]]
}
