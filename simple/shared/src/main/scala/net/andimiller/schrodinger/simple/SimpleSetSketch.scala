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
import cats.data.NonEmptyLazyList
import cats.kernel.BoundedSemilattice
import net.andimiller.schrodinger.Cardinality
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.Jaccard

import scala.collection.mutable

/** A SetSketch, as introduced in "SetSketch: Filling the Gap between MinHash and HyperLogLog" (Ertl, VLDB 2021).
  *
  * A SetSketch summarizes a set in `2^LgK` small integer registers. Adding an item hashes it once
  * per register and keeps the largest value seen in each:
  *
  *   1. read a uniform value `u ∈ (0, 1)` from the hash bits
  *   2. convert it to an exponential sample `X = −ln(u)/a`  (so `X ~ Exp(a)`)
  *   3. record `⌊1 − log₂ X⌋` — roughly "how small X is, measured in powers of two"
  *
  * With n distinct items, the smallest X a register sees is typically about `1/(n·a)`, so the
  * register lands near `log₂(n·a)`: register values grow logarithmically with set size, letting one
  * fixed-size sketch cover sets of any size. HyperLogLog registers do the same job by counting
  * leading zeros; SetSketch's exponential mapping instead yields a closed-form cardinality
  * estimator (no empirically calibrated tables) plus similarity estimation — that is what fills
  * the gap between MinHash and HyperLogLog.
  *
  * This is the "definitional" version from the paper: every item is hashed once per register, which
  * is the clearest way to demonstrate the algorithm (the paper's ordered-update variant exists for speed).
  *
  * @param registers
  *   the register values, one per bucket
  * @tparam LgK
  *   log2 of the number of registers
  */
case class SimpleSetSketch[LgK <: Int: ValueOf](registers: Vector[Int]) {
  require(
    registers.length == numRegisters,
    s"SimpleSetSketch requires 2^${valueOf[LgK]} registers, but got ${registers.length}"
  )

  lazy val numRegisters: Int = Math.pow(2, valueOf[LgK].toDouble).toInt

  /** The paper's simple cardinality estimator: n = m / (2 a ln2 · Σ 2^(−K)).
    *
    * Registers never leave zero unless an item has been added, so an all-zero sketch is exactly empty.
    */
  lazy val cardinality: Double = {
    if (registers.forall(_ == 0)) {
      0.0
    } else {
      val sum = registers.map(k => Math.pow(2.0, -k.toDouble)).sum
      numRegisters / (2 * SimpleSetSketch.a * Math.log(2) * sum)
    }
  }
}

object SimpleSetSketch {

  // rate parameter of the exponential distribution, from the paper's reference configuration;
  // any value works (the estimator accounts for it), 20 keeps registers away from zero for small sets
  private val a = 20.0

  // register values saturate here; a hash landing within 2^-63 of 1 is astronomically unlikely
  private val maxRegister = 63

  def fromItems[LgK <: Int: ValueOf, Input](
      items: NonEmptyLazyList[Input]
  )(implicit hasherFactory: HasherFactory[Int, Input, Long]): SimpleSetSketch[LgK] = {
    val numRegisters = Math.pow(2, valueOf[LgK].toDouble).toInt
    val hashers      = (0 until numRegisters).toVector.map(hasherFactory.create)

    val empty = mutable.IndexedBuffer.fill(numRegisters)(0)

    SimpleSetSketch[LgK](
      items
        .foldLeft(empty) { case (registers, item) =>
          registers.indices.foreach { idx =>
            val k = registerValue(hashers(idx).hash(item))
            if (k > registers(idx)) {
              registers(idx) = k
            }
          }
          registers
        }
        .toVector
    )
  }

  // map a 64-bit hash to a register value, following the paper's definition:
  //   u = uniform value from the hash, X = −ln(u)/a ~ Exp(a), K = ⌊1 − log2(X)⌋, clamped
  private def registerValue(hash: Long): Int = {
    val u = ((hash >>> 11) + 1).toDouble / (1L << 53)
    val x = -Math.log(u) / a
    val k = Math.floor(1 - Math.log(x) / Math.log(2)).toInt
    Math.max(0, Math.min(maxRegister, k))
  }

  /** Estimate the Jaccard similarity of two sets using inclusion-exclusion, via the derived
    * `Jaccard.fromCardinalityAndSemilattice` instance — see there for the formula and its caveats.
    */
  def jaccard[LgK <: Int: ValueOf](
      left: SimpleSetSketch[LgK],
      right: SimpleSetSketch[LgK]
  ): Double =
    Jaccard[SimpleSetSketch[LgK]].jaccard(left, right)

  implicit def boundedSemilattice[LgK <: Int: ValueOf]: BoundedSemilattice[SimpleSetSketch[LgK]] =
    new BoundedSemilattice[SimpleSetSketch[LgK]] {
      override def empty: SimpleSetSketch[LgK] =
        SimpleSetSketch(Vector.fill(Math.pow(2, valueOf[LgK].toDouble).toInt)(0))

      override def combine(
          x: SimpleSetSketch[LgK],
          y: SimpleSetSketch[LgK]
      ): SimpleSetSketch[LgK] =
        SimpleSetSketch(x.registers.zip(y.registers).map { case (a, b) => Math.max(a, b) })
    }

  implicit def eq[LgK <: Int]: Eq[SimpleSetSketch[LgK]] =
    Eq.by(_.registers)

  implicit def cardinality[LgK <: Int]: Cardinality[SimpleSetSketch[LgK]] =
    _.cardinality

  implicit def jaccardInstance[LgK <: Int: ValueOf]: Jaccard[SimpleSetSketch[LgK]] =
    Jaccard.fromCardinalityAndSemilattice
}
