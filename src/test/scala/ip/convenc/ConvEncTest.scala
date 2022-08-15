package org.datenlord
package ip.convenc

import org.scalatest.flatspec.AnyFlatSpec

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class ConvEncTest extends AnyFlatSpec {

  // FIXME: this test failed and it is currently ignored

  //  "ConvEnc" should "work" in {
  ignore should "work" in {
    val config = ConvEncConfig(Seq(Seq("171", "133")))
    SpinalConfig().generateSystemVerilog(ConvEnc(config))
  }

}
