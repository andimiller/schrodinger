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
import cats.kernel.CommutativeMonoid
import net.andimiller.schrodinger.HasherFactory

/** A Count-Min Sketch, a compact frequency estimator (Cormode & Muthukrishnan, 2005).
  *
  * It keeps a matrix of `Rows × Width` counters. Adding an item hashes it with one hash function
  * per row and increments one counter in each row; asking for an item's frequency returns the
  * smallest counter it maps to across all rows.
  *
  * Collisions can only inflate a counter, never deflate it, so the answer is always at least the
  * true count — the classic "never underestimates" property of count-min sketches.
  *
  * Unlike every other sketch in this library, the merge is element-wise addition, not min or max:
  * it counts multiplicities, so merging the same stream twice counts it twice. That makes it the
  * library's first non-idempotent sketch — a semilattice cannot represent frequencies, a
  * commutative monoid can.
  *
  * @param counters
  *   the counter matrix, one row per hash function
  * @tparam Rows
  *   number of hash functions (rows)
  * @tparam Width
  *   number of counters per row
  * @tparam Input
  *   type of the items being counted
  */
case class SimpleCountMinSketch[Rows <: Int: ValueOf, Width <: Int: ValueOf, Input](
    counters: Vector[Vector[Int]]
)(implicit hasherFactory: HasherFactory[Int, Input, Int]) {
  require(
    counters.length == valueOf[Rows] && counters.forall(_.length == valueOf[Width]),
    s"SimpleCountMinSketch requires ${valueOf[Rows]} rows of ${valueOf[Width]} counters, but got ${counters.map(_.length)}"
  )

  def add(item: Input): SimpleCountMinSketch[Rows, Width, Input] = SimpleCountMinSketch(
    counters.zipWithIndex.map { case (row, rowIndex) =>
      val cell = SimpleCountMinSketch.cell(hasherFactory.create(rowIndex).hash(item), valueOf[Width])
      row.updated(cell, row(cell) + 1)
    }
  )

  def query(item: Input): Int =
    counters.zipWithIndex.map { case (row, rowIndex) =>
      row(SimpleCountMinSketch.cell(hasherFactory.create(rowIndex).hash(item), valueOf[Width]))
    }.min
}

object SimpleCountMinSketch {

  private def cell(hash: Int, width: Int): Int = Math.floorMod(hash, width)

  def empty[Rows <: Int: ValueOf, Width <: Int: ValueOf, Input](implicit
      hasherFactory: HasherFactory[Int, Input, Int]
  ): SimpleCountMinSketch[Rows, Width, Input] =
    SimpleCountMinSketch(Vector.fill(valueOf[Rows])(Vector.fill(valueOf[Width])(0)))

  def fromItems[Rows <: Int: ValueOf, Width <: Int: ValueOf, Input](
      items: LazyList[Input]
  )(implicit hasherFactory: HasherFactory[Int, Input, Int]): SimpleCountMinSketch[Rows, Width, Input] =
    items.foldLeft(empty[Rows, Width, Input])((sketch, item) => sketch.add(item))

  implicit def commutativeMonoid[Rows <: Int: ValueOf, Width <: Int: ValueOf, Input](implicit
      hasherFactory: HasherFactory[Int, Input, Int]
  ): CommutativeMonoid[SimpleCountMinSketch[Rows, Width, Input]] =
    new CommutativeMonoid[SimpleCountMinSketch[Rows, Width, Input]] {
      override def empty: SimpleCountMinSketch[Rows, Width, Input] =
        SimpleCountMinSketch.empty[Rows, Width, Input]

      override def combine(
          x: SimpleCountMinSketch[Rows, Width, Input],
          y: SimpleCountMinSketch[Rows, Width, Input]
      ): SimpleCountMinSketch[Rows, Width, Input] =
        SimpleCountMinSketch(
          x.counters.zip(y.counters).map { case (xRow, yRow) =>
            xRow.zip(yRow).map { case (a, b) => a + b }
          }
        )
    }

  implicit def eq[Rows <: Int, Width <: Int, Input]: Eq[SimpleCountMinSketch[Rows, Width, Input]] =
    Eq.by(_.counters)
}
