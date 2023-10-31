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
import cats.implicits.*
import cats.kernel.BoundedSemilattice
import net.andimiller.schrodinger.Hasher

import scala.collection.SortedSet

/** This is a simple theta sketch intended for use as a learning material, it won't perform as well as optimised ones
  *
  * A theta sketch is a sample of the lowest N hashes we've seen, we configure this with the LgK type parameter
  *
  * @param hashes
  *   sample of hashes present in this sketch
  * @tparam LgK
  *   log of the number of items to keep
  */
case class SimpleThetaSketch[LgK <: Int: ValueOf](hashes: SortedSet[Int]) {
  lazy val maxItems: Int = Math.pow(2, valueOf[LgK].toDouble).toInt
  lazy val theta: Double =
    (((hashes.max.toDouble / Int.MaxValue.toDouble) + 1) / 2)

  lazy val cardinality: Double = {
    if (hashes.size < maxItems) { // we're still in exact mode
      hashes.size.toDouble
    } else {
      (maxItems.toDouble - 1) / theta
    }
  }
}

object SimpleThetaSketch {

  def fromItems[LgK <: Int: ValueOf, Input](
      items: LazyList[Input]
  )(implicit hasher: Hasher[Input, Int]): SimpleThetaSketch[LgK] = {
    val maxItems =
      Math
        .pow(2, valueOf[LgK].toDouble)
        .toInt // we keep items equal to 2 to the power of LgK

    BoundedSemilattice[SimpleThetaSketch[LgK]].combineAll(
      items
        .map(hasher.hash)
        .grouped(maxItems)
        .map { batch =>
          SimpleThetaSketch(SortedSet.from(batch))
        }
    )
  }

  implicit def boundedSemilattice[LgK <: Int: ValueOf]: BoundedSemilattice[SimpleThetaSketch[LgK]] =
    new BoundedSemilattice[SimpleThetaSketch[LgK]] {

      override def empty: SimpleThetaSketch[LgK] = SimpleThetaSketch(
        SortedSet.empty
      )

      override def combine(
          x: SimpleThetaSketch[LgK],
          y: SimpleThetaSketch[LgK]
      ): SimpleThetaSketch[LgK] =
        SimpleThetaSketch(
          (x.hashes ++ y.hashes).take(x.maxItems)
        )
    }

  implicit def eq[LgK <: Int]: Eq[SimpleThetaSketch[LgK]] =
    Eq.by(_.hashes.toSet)

}
