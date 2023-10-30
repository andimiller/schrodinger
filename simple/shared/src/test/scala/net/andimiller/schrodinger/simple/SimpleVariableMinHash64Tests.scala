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

import cats.data.NonEmptyLazyList
import munit.DisciplineSuite
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.HashesArbitrary
import net.andimiller.schrodinger.SimilarityHashLaws
import net.andimiller.schrodinger.SimilarityHashTests
import net.andimiller.schrodinger.simple.arb.SimpleVariableMinHash64Arbitraries
import org.scalacheck.Prop.forAll

import java.nio.ByteBuffer

class SimpleVariableMinHash64Tests
    extends DisciplineSuite
    with SimilarityHashTests[SimpleVariableMinHash64[128, 8]]
    with SimpleVariableMinHash64Arbitraries
    with HashesArbitrary {

  checkAll(
    "SimpleVariableMinHash64[128, 8]",
    similarityHash
  )

  test("Jaccard should give an expected value") {
    implicit val hasherFactory: HasherFactory[Int, String, Long] = {
      seed => str =>
        {
          val upper = HasherFactory.murmur3.create(seed).hash(str)
          val lower = HasherFactory.murmur3.create(0 - seed).hash(str)
          ByteBuffer.allocate(8).putInt(upper).putInt(lower).getLong(0)
        }
    }
    val one =
      SimpleVariableMinHash64.fromItems[4096, 16, String](
        NonEmptyLazyList("hello", "world")
      )
    val two =
      SimpleVariableMinHash64.fromItems[4096, 16, String](
        NonEmptyLazyList("hello")
      )

    assertEqualsDouble(
      SimpleVariableMinHash64.jaccard(one, two),
      0.5,
      delta = 0.05,
      "Expected jaccard to be around 0.5"
    )
  }

  property("Serialized size must be as expected") {
    forAll { (s: SimpleVariableMinHash64[1024, 16]) =>
      s.serialize.size == 1024 * 16
    }
  }

  property("Codec roundtrip") {
    forAll { (s: SimpleVariableMinHash64[32, 32]) =>
      SimpleVariableMinHash64
        .deserialize[32, 32](s.serialize)
        .toOption
        .get
        .value == s
    }
  }

  override def laws: SimilarityHashLaws[SimpleVariableMinHash64[128, 8]] =
    SimilarityHashLaws[SimpleVariableMinHash64[128, 8]]
}
