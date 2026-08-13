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

import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

trait JaccardTests[A] extends Laws {

  def jaccardLaws: JaccardLaws[A]

  def jaccard(implicit arbA: Arbitrary[A]): RuleSet =
    new RuleSet {
      val name: String                  = "jaccard"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet]         = Nil
      val props: Seq[(String, Prop)]    = Seq(
        "self similarity" -> forAll { (a: A) =>
          jaccardLaws.jaccardSelfSimilarity(a)
        },
        "symmetry"        -> forAll { (a: A, b: A) =>
          jaccardLaws.jaccardSymmetry(a, b)
        }
      )
    }

}

object JaccardTests {
  def apply[A: Jaccard]: JaccardTests[A] =
    new JaccardTests[A] {
      def jaccardLaws: JaccardLaws[A] = JaccardLaws[A]
    }
}
