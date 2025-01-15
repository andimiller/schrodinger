package net.andimiller.schrodinger.simple

import net.andimiller.schrodinger.QuantumBoolean
import net.andimiller.schrodinger.HasherFactory

import scala.collection.immutable.BitSet

case class SimpleBloomFilter[Bits <: Int: ValueOf, Input](set: BitSet)(implicit
    hasherFactory: HasherFactory[Int, Input, Int]
) {
  def add(item: Input): SimpleBloomFilter[Bits, Input] = SimpleBloomFilter(
    (0 until valueOf[Bits]).foldLeft(set) { case (s, seed) =>
      s incl hasherFactory.create(seed).hash(item)
    }
  )

  def contains(item: Input): QuantumBoolean =
    (0 until valueOf[Bits]).map { seed => hasherFactory.create(seed).hash(item) }.forall(set.contains) match {
      case true  => QuantumBoolean.Maybe
      case false => QuantumBoolean.False
    }

}

object SimpleBloomFilter {

  def empty[Bits <: Int: ValueOf, Input](implicit hasherFactory: HasherFactory[Int, Input, Int]): SimpleBloomFilter[Bits, Input] =
    SimpleBloomFilter[Bits, Input](BitSet.empty)

}
