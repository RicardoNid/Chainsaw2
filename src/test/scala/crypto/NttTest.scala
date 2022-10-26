package org.datenlord
package crypto

import org.scalatest.flatspec.AnyFlatSpec

class NttTest extends AnyFlatSpec {

  "ntt" should "work" in ChainsawTest.test(
    gen = Ntt(256,inverse = false,3329, 16),
    data = Seq.fill(512)(BigInt(1))
  )

}
