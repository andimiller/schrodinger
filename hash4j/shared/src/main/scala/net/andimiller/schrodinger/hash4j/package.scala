package net.andimiller.schrodinger

import com.dynatrace.hash4j.hashing.Hashing

package object hash4j extends UltraLogLogInstances {

  val wyhashFinal4Factory: HasherFactory[Long, String, Long] = seed =>
    (input => Hashing.wyhashFinal4(seed).hashCharsToLong(input))

  val wyhashFinal4: Hasher[String, Long] =
    (input => Hashing.wyhashFinal4().hashCharsToLong(input))

}
