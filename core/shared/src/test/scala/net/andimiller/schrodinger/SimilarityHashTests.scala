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

package net.andimiller.schrodinger

import cats.data.NonEmptyLazyList
import cats.kernel.Eq
import cats.kernel.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

trait SimilarityHashTests[A] extends SemilatticeTests[A] {

  def laws: SimilarityHashLaws[A]

  def similarityHash(implicit
      arbA: Arbitrary[A],
      arbHashes: Arbitrary[NonEmptyLazyList[Long]],
      eqA: Eq[A]
  ): RuleSet =
    new RuleSet {
      val name: String                  = "similarityHash"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet]         = Seq(semilattice)
      val props: Seq[(String, Prop)]    = Seq(
        "distributive" -> forAll { (a: NonEmptyLazyList[Long], b: NonEmptyLazyList[Long]) =>
          laws.distributive(a, b)
        }
      )
    }

}

object SimilarityHashTests {
  def apply[A: SimilarityHash]: SimilarityHashTests[A] =
    new SimilarityHashTests[A] {
      def laws: SimilarityHashLaws[A] = SimilarityHashLaws[A]
    }
}
