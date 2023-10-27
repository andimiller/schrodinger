package net.andimiller.schrodinger

import scala.util.hashing.MurmurHash3

trait HasherFactory[Seed, I, O] {
  def create(seed: Seed): Hasher[I, O]
}

object HasherFactory {
  val murmur3: HasherFactory[Int, String, Int] =
    (seed: Int) => MurmurHash3.stringHash(_, seed)
}
