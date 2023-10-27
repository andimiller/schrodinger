package net.andimiller.schrodinger.simple

import cats.data.NonEmptyLazyList
import cats.kernel.laws.SemilatticeLaws
import cats.kernel.laws.discipline.SemilatticeTests
import munit.DisciplineSuite
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.arb.SimpleMinHashArbitraries

class SimpleMinHashTests
    extends DisciplineSuite
    with SemilatticeTests[SimpleMinHash[32]]
    with SimpleMinHashArbitraries {

  checkAll(
    "SimpleMinHash[32]",
    semilattice
  )

  test("Jaccard should give an expected value") {
    implicit val hasherFactory: HasherFactory[Int, String, Int] =
      HasherFactory.murmur3
    val one =
      SimpleMinHash.fromItems[1024, String](NonEmptyLazyList("hello", "world"))
    val two =
      SimpleMinHash.fromItems[1024, String](NonEmptyLazyList("hello"))

    assertEqualsDouble(
      SimpleMinHash.jaccard(one, two),
      0.5,
      delta = 0.03,
      "Expected jaccard to be around 0.5"
    )
  }

  override def laws: SemilatticeLaws[SimpleMinHash[32]] =
    SemilatticeLaws[SimpleMinHash[32]]
}
