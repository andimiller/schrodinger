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
import net.andimiller.schrodinger.Hasher
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.SimplePRNGMinHash
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import java.nio.ByteBuffer

trait SimplePRNGMinHashArbitraries {

  implicit val hasher: Hasher[String, Long] = { str =>
    {
      val upper = HasherFactory.murmur3.create(0).hash(str)
      val lower = HasherFactory.murmur3.create(upper).hash(str)
      ByteBuffer.allocate(8).putInt(upper).putInt(lower).getLong(0)
    }
  }
  implicit def simplePRNGMinHashArb[
      HashCount <: Int: ValueOf,
      HashWidth <: Int: ValueOf
  ]: Arbitrary[SimplePRNGMinHash[HashCount, HashWidth]] = {

    Arbitrary(
      Gen
        .nonEmptyListOf(Gen.alphaNumStr)
        .map(ss => NonEmptyLazyList.fromSeq(ss).get)
        .map(
          SimplePRNGMinHash.fromItems[HashCount, HashWidth, String](_)
        )
    )
  }

}
