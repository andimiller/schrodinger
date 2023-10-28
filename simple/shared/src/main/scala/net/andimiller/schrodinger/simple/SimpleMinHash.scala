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
import cats.data.{NonEmptyLazyList, NonEmptyLazyListOps}
import cats.kernel.{BoundedSemilattice, Semilattice}
import net.andimiller.schrodinger.HasherFactory

/** An implementation of MinHash using 32-bit hashes stored in a Vector
  *
  * This exists as an example of how to implement MinHash, written in the
  * clearest way possible to demonstrate the algorithm, this may lead to bad
  * performance.
  *
  * @param hashes
  *   the hashes stored
  * @tparam HashCount
  *   Number of hash variants to store, this should be the length of the Vector
  */
case class SimpleMinHash[HashCount <: Int: ValueOf](hashes: Vector[Int]) {}

object SimpleMinHash {

  def fromItems[HashCount <: Int: ValueOf, Input](
      items: NonEmptyLazyList[Input]
  )(implicit hasherFactory: HasherFactory[Int, Input, Int]) = {
    val hashers = (0 to valueOf[HashCount]).toVector
      .map(hasherFactory.create)

    items.map { item =>
      SimpleMinHash[HashCount](hashers.map(_.hash(item)))
    }.reduce
  }

  def jaccard[HashCount <: Int: ValueOf](
      left: SimpleMinHash[HashCount],
      right: SimpleMinHash[HashCount]
  ): Double = {
    val i = (left.hashes zip right.hashes).count { case (l, r) => l == r }
    val u = valueOf[HashCount]
    i.toDouble / u.toDouble
  }

  // semigroup we use below
  private val unsignedIntOrdering: Ordering[Int] = Ordering.fromLessThan {
    case (a, b) => Integer.compareUnsigned(a, b) < 0
  }

  // when combining minhashes, we take the minimum hash for each index
  implicit def semilattice[HashCount <: Int: ValueOf]
      : Semilattice[SimpleMinHash[HashCount]] =
    (x: SimpleMinHash[HashCount], y: SimpleMinHash[HashCount]) =>
      SimpleMinHash(
        x.hashes.zip(y.hashes).map { case (l, r) =>
          unsignedIntOrdering.min(l, r)
        }
      )

  implicit def eq[HashCount <: Int: ValueOf]: Eq[SimpleMinHash[HashCount]] =
    Eq.by(_.hashes)

}
