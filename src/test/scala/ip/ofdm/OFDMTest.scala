package org.datenlord
package ip.ofdm

import org.scalatest.flatspec.AnyFlatSpec

class OFDMTest extends AnyFlatSpec {

  "OFDM" should "work" in {

    val config = OFDMConfig(1)
    VivadoSynth(OFDM(config))
  }

}
