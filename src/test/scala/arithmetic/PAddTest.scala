package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec

class PAddTest extends AnyFlatSpec {

  "PAdd" should "print" in PAdd().toPng("padd")

  it should "gen" in RtlGen(PAdd().toTransform)

  it should "impl" in VivadoImpl(PAdd().toTransform, "padd")

}
