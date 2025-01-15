package net.andimiller.schrodinger

import cats.implicits.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import spire.algebra.{Bool, Eq}
import spire.laws.InvalidTestException._
import spire.syntax.eq._
import spire.syntax.bool._
import org.scalacheck.Prop._
import spire.laws.LogicLaws

class QuantumBooleanLaws extends DisciplineSuite with QuantumBooleanArbitrary with LogicLaws[QuantumBoolean] {
  override implicit def Equ: Eq[QuantumBoolean]        = QuantumBoolean.eq
  override implicit def Arb: Arbitrary[QuantumBoolean] = quantumBooleanArbitrary

  def qbool[A: Eq: Arbitrary](implicit A: Bool[A]) =
    new DefaultRuleSet(
      name = "qbool",
      parent = None,
      "and True"  -> forAllSafe { (x: A) => (x & A.one) == x },
      "and False" -> forAllSafe { (x: A) => (x & A.zero) == A.zero },
      "or True"   -> forAllSafe { (x: A) => (x | A.one) == A.one },
      "or False"  -> forAllSafe { (x: A) => (x | A.zero) == x },
      "xor"       -> forAllSafe { (a: A, b: A) => (a ^ b) === ((a & ~b) | (~a & b)) },
      "nxor"      -> forAllSafe { (a: A, b: A) => (a.nxor(b)) === ((a | ~b) & (~a | b)) },
      "imp"       -> forAllSafe { (a: A, b: A) => (a.imp(b)) === (~a | b) },
      "nand"      -> forAllSafe { (a: A, b: A) => (a.nand(b)) === ~(a & b) },
      "nor"       -> forAllSafe { (a: A, b: A) => (a.nor(b)) === ~(a | b) }
    )

  checkAll("QuantumBoolean", qbool[QuantumBoolean])
}
