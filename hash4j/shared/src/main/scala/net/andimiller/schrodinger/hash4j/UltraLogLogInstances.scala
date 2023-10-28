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

import cats.Eq
import cats.kernel.BoundedSemilattice
import com.dynatrace.hash4j.distinctcount.UltraLogLog

trait UltraLogLogInstances {
  // you'll need to provide an implicit one of these to get instances, since we need to know the P value
  case class UltraLogLogP(value: Int)

  implicit def ullBoundedSemilattice(implicit
      p: UltraLogLogP
  ): BoundedSemilattice[UltraLogLog] = new BoundedSemilattice[UltraLogLog] {
    override def empty: UltraLogLog = UltraLogLog.create(p.value)
    override def combine(x: UltraLogLog, y: UltraLogLog): UltraLogLog = {
      val merged = UltraLogLog.create(p.value)
      merged.add(x)
      merged.add(y)
      merged
    }
  }

  implicit val ullEq: Eq[UltraLogLog] = new Eq[UltraLogLog] {
    override def eqv(x: UltraLogLog, y: UltraLogLog): Boolean =
      (x.getP == y.getP) && (x.getState sameElements y.getState)
  }
}
