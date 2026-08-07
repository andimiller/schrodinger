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
import net.andimiller.schrodinger.simple.SimpleSetSketch
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import java.nio.ByteBuffer

trait SimpleSetSketchArbitraries {

  implicit def simpleSetSketchArbitraries[LgK <: Int: ValueOf]: Arbitrary[SimpleSetSketch[LgK]] = {
    implicit val hasherFactory: HasherFactory[Int, String, Long] = { seed => str =>
      {
        val upper = HasherFactory.murmur3.create(seed).hash(str)
        val lower = HasherFactory.murmur3.create(seed ^ 0x9e3779b9).hash(str)
        ByteBuffer.allocate(8).putInt(upper).putInt(lower).getLong(0)
      }
    }
    Arbitrary(
      Gen
        .nonEmptyListOf(Gen.alphaNumStr)
        .map(list => SimpleSetSketch.fromItems[LgK, String](NonEmptyLazyList.fromSeq(list).get))
    )
  }

}
