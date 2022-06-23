package org.datenlord
package ip.das

import spinal.core._

case class LvdsDebug() extends Component {

  // I/O assigned
  // ADC相关
  val adc_clk = in Bool() // 250MHz
  val rstn = in Bool()
  val adcInterface = AdcInterface()
  adcInterface.setName("")
  val dout_a, dout_b = out Vec(UInt(14 bits), 4) // LVDS signal out for user logic

  val signalAcq = SignalAcq()
  val p2sA = P2SCC(14, 4)

  // Adc信号采集
  adcInterface <> signalAcq.adcInterface
  dout_a := signalAcq.dout_a
  dout_b := signalAcq.dout_b
  signalAcq.clkRef := adc_clk
  signalAcq.rstn := rstn

  // Adc信号合成
  p2sA.clkRef := signalAcq.clkOut
  p2sA.rstn := rstn
  p2sA.dataIns.zip(signalAcq.dout_a).foreach { case (port, data) => port := data.asBits }
  val dout_a_combined = out UInt (14 bits)
  dout_a_combined := p2sA.dataOut.asUInt

  // 控制输入的可调放大器
  val VGAMAX = 0
  val VGAMIN = 63
  val vga_b = out UInt (6 bits) // control the amplitude of the amplifier
  vga_b := U(VGAMIN, 6 bits)

}

object LvdsDebug {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "/home/ltr/sysudas/project/LvdsDebug").generateVerilog(LvdsDebug())
  }
}
