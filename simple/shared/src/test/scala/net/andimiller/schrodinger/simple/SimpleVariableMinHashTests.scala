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
import net.andimiller.schrodinger.JaccardLaws
import net.andimiller.schrodinger.JaccardTests
import net.andimiller.schrodinger.SimilarityHashLaws
import net.andimiller.schrodinger.SimilarityHashTests
import net.andimiller.schrodinger.simple.arb.SimpleVariableMinHashArbitraries
import org.scalacheck.Prop.forAll

import java.nio.ByteBuffer

class SimpleVariableMinHashTests
    extends DisciplineSuite
    with SimilarityHashTests[SimpleVariableMinHash[128, 8]]
    with JaccardTests[SimpleVariableMinHash[128, 8]]
    with SimpleVariableMinHashArbitraries
    with HashesArbitrary {

  checkAll(
    "SimpleVariableMinHash[128, 8]",
    similarityHash
  )

  checkAll(
    "SimpleVariableMinHash[128, 8] jaccard",
    jaccard
  )

  test("Jaccard should give an expected value") {
    implicit val hasherFactory: HasherFactory[Int, String, Int] =
      HasherFactory.murmur3
    val one                                                     =
      SimpleVariableMinHash.fromItems[4096, 16, String, Int](
        NonEmptyLazyList("hello", "world")
      )
    val two                                                     =
      SimpleVariableMinHash.fromItems[4096, 16, String, Int](
        NonEmptyLazyList("hello")
      )

    assertEqualsDouble(
      SimpleVariableMinHash.jaccard(one, two),
      0.5,
      delta = 0.05,
      "Expected jaccard to be around 0.5"
    )
  }

  test("fromItems should accept a 64-bit hasher with a HashWidth above 32") {
    implicit val hasherFactory: HasherFactory[Int, String, Long] = { seed => str =>
      {
        val upper = HasherFactory.murmur3.create(seed).hash(str)
        val lower = HasherFactory.murmur3.create(seed ^ 0x9e3779b9).hash(str)
        ByteBuffer.allocate(8).putInt(upper).putInt(lower).getLong(0)
      }
    }

    val result =
      SimpleVariableMinHash.fromItems[16, 48, String, Long](
        NonEmptyLazyList("hello", "world")
      )

    assertEquals(result.hashes.size, 16)
  }

  property("Serialized size must be as expected") {
    forAll { (s: SimpleVariableMinHash[1024, 16]) =>
      s.serialize.size == 1024 * 16
    }
  }

  property("Codec roundtrip") {
    forAll { (s: SimpleVariableMinHash[32, 32]) =>
      SimpleVariableMinHash
        .deserialize[32, 32](s.serialize)
        .toOption
        .get
        .value == s
    }
  }

  override def laws: SimilarityHashLaws[SimpleVariableMinHash[128, 8]] =
    SimilarityHashLaws[SimpleVariableMinHash[128, 8]]

  override def jaccardLaws: JaccardLaws[SimpleVariableMinHash[128, 8]] =
    JaccardLaws[SimpleVariableMinHash[128, 8]]
}
