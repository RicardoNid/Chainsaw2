package org.datenlord
package ip.das

import spinal.core._

case class DasWhole() extends Component {

  // pins on board
  // ADC相关
  val adc_clk = in Bool() // 250MHz
  val rstn = in Bool()
  val adcInterface = AdcInterface()
  adcInterface.setName("")
  // PCIe相关
  val pcieInterface = PcieBundle()
  // 控制输入的可调放大器
  val vga_b = out UInt(6 bits)

  // modules and top-down connections
  val signalAcq = SignalAcq()
  adcInterface <> signalAcq.adcInterface
  signalAcq.clkRef := adc_clk
  signalAcq.rstn := rstn
  val p2sA = P2SCC(14, 4)
  p2sA.rstn := rstn
  val signalPro = SignalPro()
  signalPro.rstn := rstn
  vga_b := signalPro.gain
  val host = Host()
  host.rstn := rstn
  pcieInterface <> host.pcieInterface

  // side-by-side connections
  // signalAcq -> P2S
  p2sA.clkRef := signalAcq.clkOut
  p2sA.dataIns.zip(signalAcq.dout_a).foreach{ case (port, data) => port := data.asBits}
  // P2S -> signalPro
  signalPro.clkRef := p2sA.clkOut
  signalPro.dataIn := p2sA.dataOut
  // signalPro -> Host
  host.clkRef := p2sA.clkOut
  signalPro.userInterface <> host.hostInterface

  setDefinitionName("xillydemo")
}

object DasWhole {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "/home/ltr/sysudas/project/xillybase/verilog/src").generateVerilog(DasWhole())
  }
}
