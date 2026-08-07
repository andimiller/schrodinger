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

import cats.kernel.Monoid
import cats.kernel.laws.BoundedSemilatticeLaws
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import com.dynatrace.hash4j.distinctcount.HyperLogLog
import com.dynatrace.hash4j.hashing.Hashing
import munit.DisciplineSuite
import net.andimiller.schrodinger.hash4j.arb.HyperLogLogArbitraries

class HyperLogLogTests
    extends DisciplineSuite
    with BoundedSemilatticeTests[HyperLogLog]
    with HyperLogLogArbitraries {

  implicit val p: HyperLogLogP = HyperLogLogP(12)

  checkAll(
    "HyperLogLog",
    boundedSemilattice
  )

  test("Cardinality should give an expected value") {
    implicit val p: HyperLogLogP = HyperLogLogP(14)
    val hll                      = Monoid[HyperLogLog].empty
    val wyhash                   = Hashing.wyhashFinal4()

    LazyList.range(0, 10000).map(_.toString).foreach { s =>
      hll.add(wyhash.hashCharsToLong(s))
    }

    assertEqualsDouble(
      hll.getDistinctCountEstimate(),
      10000,
      delta = 500,
      "Expected cardinality to be around 10000"
    )
  }

  override def laws: BoundedSemilatticeLaws[HyperLogLog] =
    BoundedSemilatticeLaws[HyperLogLog]
}
