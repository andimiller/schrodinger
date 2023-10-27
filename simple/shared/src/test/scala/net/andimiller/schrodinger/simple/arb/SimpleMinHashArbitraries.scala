package net.andimiller.schrodinger.simple.arb

import cats.data.NonEmptyLazyList
import net.andimiller.schrodinger.HasherFactory
import net.andimiller.schrodinger.simple.SimpleMinHash
import org.scalacheck.{Arbitrary, Gen}

trait SimpleMinHashArbitraries {

  implicit def simpleMinHashArb[HashCount <: Int: ValueOf]
      : Arbitrary[SimpleMinHash[HashCount]] = {
    implicit val hasherFactory: HasherFactory[Int, String, Int] =
      HasherFactory.murmur3
    Arbitrary(
      Gen
        .nonEmptyListOf(Gen.alphaNumStr)
        .map(ss => NonEmptyLazyList.fromSeq(ss).get)
        .map(SimpleMinHash.fromItems[HashCount, String](_))
    )
  }

}
