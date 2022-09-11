package org.datenlord
package ip.das

import spinal.core._

import scala.language.postfixOps

case class SignalAcqAnother() extends BlackBox {
  val adc_clk, BUF_CLK, rstn = in Bool()
  val adcIns = AdcIns() // ADC
  val ddsOuts = DdsOuts() // DDS
  // Data Acquired
  val ref_clk_out = out Bool() // clock of data acquired 62.5MHz
  val DOUTA, DOUTB, DOUTC, DOUTD = out Bits(14 bits)
  val DOUTBA, DOUTBB, DOUTBC, DOUTBD = out Bits(14 bits)

  setDefinitionName("SignalAcq")
}
