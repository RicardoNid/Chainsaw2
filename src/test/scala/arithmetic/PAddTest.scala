package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec

class PAddTest extends AnyFlatSpec {

  "PAdd" should "print" in PAdd().toPng("padd")

  it should "gen" in RtlGen(PAdd().toTransform, "padd")

  it should "impl" in VivadoImpl(PAdd().toTransform, "padd")

  it should "impl for karatsuba" in VivadoImpl(Karatsuba377().toTransform)

  //  it should "impl for MSB mult" in

}
