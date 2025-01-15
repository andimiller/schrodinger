package net.andimiller.schrodinger

import net.andimiller.schrodinger.QuantumBoolean.{Maybe, True, False}
import org.scalacheck.{Arbitrary, Gen}

trait QuantumBooleanArbitrary {
  implicit val quantumBooleanArbitrary: Arbitrary[QuantumBoolean] = Arbitrary(
    Gen.oneOf(True, Maybe, False)
  )
}
