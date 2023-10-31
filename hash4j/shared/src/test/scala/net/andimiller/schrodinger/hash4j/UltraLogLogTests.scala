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

import cats.kernel.laws.BoundedSemilatticeLaws
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import com.dynatrace.hash4j.distinctcount.UltraLogLog
import munit.DisciplineSuite
import net.andimiller.schrodinger.hash4j.arb.UltraLogLogArbitraries

class UltraLogLogTests extends DisciplineSuite with BoundedSemilatticeTests[UltraLogLog] with UltraLogLogArbitraries {

  implicit val p: UltraLogLogP = UltraLogLogP(12)

  checkAll(
    "UltraLogLog",
    boundedSemilattice
  )

  override def laws: BoundedSemilatticeLaws[UltraLogLog] =
    BoundedSemilatticeLaws[UltraLogLog]
}
