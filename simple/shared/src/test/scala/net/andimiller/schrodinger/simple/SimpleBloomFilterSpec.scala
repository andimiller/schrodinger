package net.andimiller.schrodinger.simple

import net.andimiller.schrodinger.{HashTruncator, HasherFactory, QuantumBoolean}

class SimpleBloomFilterTests extends munit.FunSuite {

  test("bloom filter should work with simple stuff") {
    implicit val hasher: HasherFactory[Int, String, Int] =
      HasherFactory.murmur3.map(HashTruncator.hashTruncator32[9].run(_).toInt(signed = false))

    val bloom = SimpleBloomFilter
      .empty[256, String]
      .add("hello")
      .add("world")

    assertEquals(
      bloom.contains("hello"),
      QuantumBoolean.Maybe
    )

    assertEquals(
      bloom.contains("goodbye"),
      QuantumBoolean.False
    )
  }

}
