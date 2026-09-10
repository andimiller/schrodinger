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

import net.andimiller.schrodinger.Hasher
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.SimpleCvmSketch
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait SimpleCvmSketchArbitraries {

  implicit def simpleCvmSketchArbitraries[LgK <: Int: ValueOf]: Arbitrary[SimpleCvmSketch[LgK]] = {
    implicit val hasher: Hasher[String, Int] =
      HasherFactory.murmur3.create(0)
    Arbitrary(
      Gen
        .listOf(Gen.alphaNumStr)
        .map(list => SimpleCvmSketch.fromItems[LgK, String](LazyList.from(list)))
    )
  }

}
