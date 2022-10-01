package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec

class Barrett377Test extends AnyFlatSpec {

  val k = 377
  val M = algos.ZPrizeMSM.baseModulus

  "barrett graph" should "gen" in RtlGen(Barrett377(k, M).toTransform)
  "barrett graph" should "impl" in VivadoImpl(Barrett377(k, M).toTransform, "barrettModularMultiplier")

}
