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

import cats.Eq
import cats.kernel.BoundedSemilattice
import net.andimiller.schrodinger.QuantumBoolean
import net.andimiller.schrodinger.HasherFactory

import scala.collection.immutable.BitSet

/** A simple bloom filter backed by a BitSet of exactly `Bits` bits.
  *
  * Each seed of the hasher produces one bit index, reduced into `[0, Bits)`.
  *
  * Merging two filters is element-wise OR — the filter of the union — which makes this a
  * `BoundedSemilattice`. The merge is only meaningful when both filters use the same hash
  * functions and the same `Bits`; the type enforces the latter, the shared implicit hasher
  * the former.
  *
  * @tparam Bits
  *   number of bits in the filter (also the number of hash functions used)
  */
case class SimpleBloomFilter[Bits <: Int: ValueOf, Input](set: BitSet)(implicit
    hasherFactory: HasherFactory[Int, Input, Int]
) {
  def add(item: Input): SimpleBloomFilter[Bits, Input] = SimpleBloomFilter(
    (0 until valueOf[Bits]).foldLeft(set) { case (s, seed) =>
      s incl Math.floorMod(hasherFactory.create(seed).hash(item), valueOf[Bits])
    }
  )

  def contains(item: Input): QuantumBoolean =
    (0 until valueOf[Bits]).map { seed => Math.floorMod(hasherFactory.create(seed).hash(item), valueOf[Bits]) }.forall(set.contains) match {
      case true  => QuantumBoolean.Maybe
      case false => QuantumBoolean.False
    }

}

object SimpleBloomFilter {

  def empty[Bits <: Int: ValueOf, Input](implicit hasherFactory: HasherFactory[Int, Input, Int]): SimpleBloomFilter[Bits, Input] =
    SimpleBloomFilter[Bits, Input](BitSet.empty)

  implicit def boundedSemilattice[Bits <: Int: ValueOf, Input](implicit
      hasherFactory: HasherFactory[Int, Input, Int]
  ): BoundedSemilattice[SimpleBloomFilter[Bits, Input]] =
    new BoundedSemilattice[SimpleBloomFilter[Bits, Input]] {
      override def empty: SimpleBloomFilter[Bits, Input] =
        SimpleBloomFilter.empty[Bits, Input]

      override def combine(
          x: SimpleBloomFilter[Bits, Input],
          y: SimpleBloomFilter[Bits, Input]
      ): SimpleBloomFilter[Bits, Input] =
        SimpleBloomFilter(x.set | y.set)
    }

  implicit def eq[Bits <: Int, Input]: Eq[SimpleBloomFilter[Bits, Input]] =
    Eq.by(_.set)
}
