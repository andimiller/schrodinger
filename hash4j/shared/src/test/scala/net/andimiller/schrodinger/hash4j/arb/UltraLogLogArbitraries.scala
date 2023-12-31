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

package net.andimiller.schrodinger.hash4j.arb

import cats.kernel.Monoid
import com.dynatrace.hash4j.distinctcount.UltraLogLog
import com.dynatrace.hash4j.hashing.Hashing
import net.andimiller.schrodinger.hash4j.UltraLogLogInstances
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait UltraLogLogArbitraries extends UltraLogLogInstances {

  implicit def simpleULLArb(implicit
      p: UltraLogLogP
  ): Arbitrary[UltraLogLog] = {
    val wyhash = Hashing.wyhashFinal4()
    Arbitrary(
      Gen
        .nonEmptyListOf(Gen.alphaNumStr)
        .map { strings =>
          val ull = Monoid[UltraLogLog].empty
          strings.foreach { s =>
            ull.add(wyhash.hashCharsToLong(s))
          }
          ull
        }
    )
  }

}
