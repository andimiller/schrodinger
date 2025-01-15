package net.andimiller.schrodinger

import spire.algebra.{Bool, Eq}

import scala.math.Ordering.Implicits.infixOrderingOps

/** Represents a boolean that has an unknown in the middle
  */
sealed trait QuantumBoolean
object QuantumBoolean {
  final case object True  extends QuantumBoolean
  final case object Maybe extends QuantumBoolean
  final case object False extends QuantumBoolean

  implicit val eq: Eq[QuantumBoolean] = Eq.fromUniversalEquals[QuantumBoolean]

  implicit val ord: Ordering[QuantumBoolean] = Ordering.by[QuantumBoolean, Int] {
    case True  => 2
    case Maybe => 1
    case False => 0
  }

  implicit val bool: Bool[QuantumBoolean] = new Bool[QuantumBoolean] {
    override def zero: QuantumBoolean                                      = False
    override def one: QuantumBoolean                                       = True
    override def complement(a: QuantumBoolean): QuantumBoolean             = a match {
      case True  => False
      case Maybe => Maybe
      case False => True
    }
    override def and(a: QuantumBoolean, b: QuantumBoolean): QuantumBoolean = (a min b)
    override def or(a: QuantumBoolean, b: QuantumBoolean): QuantumBoolean  = (a max b)
  }
}
