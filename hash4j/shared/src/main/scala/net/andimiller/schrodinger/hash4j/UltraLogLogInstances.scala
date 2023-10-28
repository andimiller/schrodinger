package net.andimiller.schrodinger.hash4j

import cats.Eq
import cats.kernel.BoundedSemilattice
import com.dynatrace.hash4j.distinctcount.UltraLogLog

trait UltraLogLogInstances {
  // you'll need to provide an implicit one of these to get instances, since we need to know the P value
  case class UltraLogLogP(value: Int)

  implicit def boundedSemilattice(implicit
      p: UltraLogLogP
  ): BoundedSemilattice[UltraLogLog] = new BoundedSemilattice[UltraLogLog] {
    override def empty: UltraLogLog = UltraLogLog.create(p.value)
    override def combine(x: UltraLogLog, y: UltraLogLog): UltraLogLog = {
      val merged = UltraLogLog.create(p.value)
      merged.add(x)
      merged.add(y)
      merged
    }
  }

  implicit val eq: Eq[UltraLogLog] = new Eq[UltraLogLog] {
    override def eqv(x: UltraLogLog, y: UltraLogLog): Boolean =
      (x.getP == y.getP) && (x.getState sameElements y.getState)
  }
}
