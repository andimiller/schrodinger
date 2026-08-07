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
import com.dynatrace.hash4j.distinctcount.HyperLogLog

trait HyperLogLogInstances {
  // you'll need to provide an implicit one of these to get instances, since we need to know the P value
  case class HyperLogLogP(value: Int)

  implicit def hllBoundedSemilattice(implicit
      p: HyperLogLogP
  ): BoundedSemilattice[HyperLogLog] = new BoundedSemilattice[HyperLogLog] {
    override def empty: HyperLogLog                                   = HyperLogLog.create(p.value)
    override def combine(x: HyperLogLog, y: HyperLogLog): HyperLogLog = {
      val merged = HyperLogLog.create(p.value)
      merged.add(x)
      merged.add(y)
      merged
    }
  }

  implicit val hllEq: Eq[HyperLogLog] = new Eq[HyperLogLog] {
    override def eqv(x: HyperLogLog, y: HyperLogLog): Boolean =
      (x.getP == y.getP) && (x.getState sameElements y.getState)
  }
}
