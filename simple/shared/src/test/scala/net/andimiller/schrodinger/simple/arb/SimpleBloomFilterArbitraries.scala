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

import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.SimpleBloomFilter
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait SimpleBloomFilterArbitraries {

  implicit def simpleBloomFilterArb[Bits <: Int: ValueOf]: Arbitrary[SimpleBloomFilter[Bits, String]] = {
    implicit val hasherFactory: HasherFactory[Int, String, Int] =
      HasherFactory.murmur3
    Arbitrary(
      Gen
        .listOf(Gen.alphaNumStr)
        .map(list => list.foldLeft(SimpleBloomFilter.empty[Bits, String])((filter, s) => filter.add(s)))
    )
  }

}
