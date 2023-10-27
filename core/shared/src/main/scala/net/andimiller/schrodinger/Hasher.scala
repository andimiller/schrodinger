package net.andimiller.schrodinger

import cats._
import cats.arrow.Profunctor

import scala.util.hashing.MurmurHash3

trait Hasher[I, O] {
  def hash(i: I): O
}

object Hasher {
  // aliases
  type Hasher32[I] = Hasher[I, Int]
  type Hasher64[I] = Hasher[I, Long]

  // type class instances
  implicit val profunctor: Profunctor[Hasher]                = new Profunctor[Hasher] {
    override def dimap[A, B, C, D](fab: Hasher[A, B])(f: C => A)(g: B => D): Hasher[C, D] =
      c => g(fab.hash(f(c)))
  }
  implicit def functor[I]: Functor[Hasher[I, *]]             = new Functor[Hasher[I, *]] {
    override def map[A, B](fa: Hasher[I, A])(f: A => B): Hasher[I, B] =
      i => f(fa.hash(i))
  }
  implicit def contravariant[O]: Contravariant[Hasher[*, O]] = new Contravariant[Hasher[*, O]] {
    override def contramap[A, B](fa: Hasher[A, O])(f: B => A): Hasher[B, O] =
      i => fa.hash(f(i))
  }

  // instances
  val murmur32: Hasher[String, Int] = s => MurmurHash3.stringHash(s)
}
