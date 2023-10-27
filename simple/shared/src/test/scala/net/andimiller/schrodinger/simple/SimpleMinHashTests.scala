package net.andimiller.schrodinger.simple

import cats.kernel.laws.SemilatticeLaws
import cats.kernel.laws.discipline.SemilatticeTests
import munit.DisciplineSuite
import net.andimiller.schrodinger.simple.arb.SimpleMinHashArbitraries

class SimpleMinHashTests
    extends DisciplineSuite
    with SemilatticeTests[SimpleMinHash[32]]
    with SimpleMinHashArbitraries {

  checkAll(
    "SimpleMinHash[32]",
    semilattice
  )

  override def laws: SemilatticeLaws[SimpleMinHash[32]] =
    SemilatticeLaws[SimpleMinHash[32]]
}
