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
import net.andimiller.schrodinger.simple.SimpleMinHash
import org.scalacheck.{Arbitrary, Gen}

trait SimpleMinHashArbitraries {

  implicit def simpleMinHashArb[HashCount <: Int: ValueOf]
      : Arbitrary[SimpleMinHash[HashCount]] = {
    implicit val hasherFactory: HasherFactory[Int, String, Int] =
      HasherFactory.murmur3
    Arbitrary(
      Gen
        .nonEmptyListOf(Gen.alphaNumStr)
        .map(ss => NonEmptyLazyList.fromSeq(ss).get)
        .map(SimpleMinHash.fromItems[HashCount, String](_))
    )
  }

}
