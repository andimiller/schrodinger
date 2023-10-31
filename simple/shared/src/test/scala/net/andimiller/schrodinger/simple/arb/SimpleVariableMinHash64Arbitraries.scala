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

package net.andimiller.schrodinger.simple.arb

import cats.data.NonEmptyLazyList
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.SimpleVariableMinHash64
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import java.nio.ByteBuffer

trait SimpleVariableMinHash64Arbitraries {

  implicit def simpleMinHashArb[
      HashCount <: Int: ValueOf,
      HashWidth <: Int: ValueOf
  ]: Arbitrary[SimpleVariableMinHash64[HashCount, HashWidth]] = {
    implicit val hasherFactory: HasherFactory[Int, String, Long] = { seed => str =>
      {
        val upper = HasherFactory.murmur3.create(seed).hash(str)
        val lower = HasherFactory.murmur3.create(0 - seed).hash(str)
        ByteBuffer.allocate(8).putInt(upper).putInt(lower).getLong(0)
      }
    }
    Arbitrary(
      Gen
        .nonEmptyListOf(Gen.alphaNumStr)
        .map(ss => NonEmptyLazyList.fromSeq(ss).get)
        .map(
          SimpleVariableMinHash64.fromItems[HashCount, HashWidth, String](
            _
          )
        )
    )
  }

}
