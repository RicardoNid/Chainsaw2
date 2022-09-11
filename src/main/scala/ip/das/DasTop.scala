package org.datenlord
package ip.das

import spinal.core._

import scala.language.postfixOps

/** pcie-related signals on a board, corresponding pin locations are needed
 *
 */
case class PcieBundle() extends Bundle {
  val perstn, refclk = in Bool()
  val rx = in Bits (4 bits)
  val tx = out Bits (4 bits)
  this.setName("pcie")
}

// ADC输出,采用DDR时序,在上升/下降沿的一对7位数据组成一个14位输出
case class AdcIns() extends Bundle {
  val adc_in_a, adc_in_b = in UInt (7 bits)
  this.setName("")
}

case class DdsOuts() extends Bundle {
  val DDSP, DDS_SDIO = out Bits (4 bits)
  val DDS_RST, DDS_CS, DDS_SCLK, DDS_IOUPDATE = out Bool()
  this.setName("")
}

case class PulsesOut() extends Bundle {
  val Pulseout0, Pulseout0N, Pulseout1, Pulseout1N, Pulse_Single = out Bool()
  this.setName("")
}

case class DasTop() extends Component {

  /** --------
   * I/O
   * -------- */
  val adc_clk, BUF_CLK, rstn = in Bool()
  val adcIns = AdcIns()
  val ddsOuts = DdsOuts()
  // PCIe相关
  val pcie = PcieBundle()
  // 可调放大器控制
  val vga_b = out UInt (6 bits)
  // 脉冲输出
  val pulsesOut = PulsesOut()
  // GPIO
  //  val RS232_RX = in Bool()
  //  val RS232_TX = out Bool()

  /** --------
   * sub-modules instantiation, connect them with clk, rst, and top level pins
   * -------- */

  // signalAcq instantiation
  val signalAcq = SignalAcq()
  signalAcq.adc_clk := adc_clk
  signalAcq.BUF_CLK := BUF_CLK
  signalAcq.rstn := rstn
  signalAcq.adcIns := adcIns
  ddsOuts := signalAcq.ddsOuts

  // signalPro instantiation
  val signalProWrapper = SignalProWrapper()
  signalProWrapper.rstn := rstn
  pulsesOut := signalProWrapper.pulsesOut
  vga_b := signalProWrapper.gainOut

  // xillybus instantiation
  val xillybusWrapper = XillybusWrapper(DasXillybusDevices())
  xillybusWrapper.pcie <> pcie

  /** --------
   * inter-submodules connections
   * -------- */

  /** --------
   * signalAcq -> signalPro
   * -------- */
  signalProWrapper.clkIn := signalAcq.ref_clk_out // 62.5MHz
  val adcData0 = Vec(signalAcq.DOUTA, signalAcq.DOUTB, signalAcq.DOUTC, signalAcq.DOUTD)
  val adcData1 = Vec(signalAcq.DOUTBA, signalAcq.DOUTBB, signalAcq.DOUTBC, signalAcq.DOUTBD)
  signalProWrapper.adcData0 := adcData0
  signalProWrapper.adcData1 := adcData1

  /** --------
   * signalPro <-> xillybus
   * -------- */
  xillybusWrapper.userClk := signalProWrapper.clkOut // 125MHz

  // idle channels
  val idleDevices = Seq("info_0_8", "info_1_8")
  idleDevices.foreach { name =>
    xillybusWrapper.getWriteInterfaceByName(name).wrreq := True
    xillybusWrapper.getWriteInterfaceByName(name).data.assignDontCare()
  }

  xillybusWrapper.getReadInterfaceByName("write_16").rdreq := True

  // channel in use
  val adc_0_16 = xillybusWrapper.getWriteInterfaceByName("adc_0_16")
  adc_0_16.wrreq := signalProWrapper.dataOut0.valid
  adc_0_16.data := signalProWrapper.dataOut0.payload

  val adc_1_16 = xillybusWrapper.getWriteInterfaceByName("adc_1_16")
  adc_1_16.wrreq := signalProWrapper.dataOut1.valid
  adc_1_16.data := signalProWrapper.dataOut1.payload

  // ctrl channel
  signalProWrapper.ctrlIn := xillybusWrapper.ctrlOut

  setDefinitionName("xillydemo")
}

object DasTop {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "xillydemo.v",
      targetDirectory = "/home/ltr/sysudas/project/xillyfinal/verilog/src")
      .generateVerilog(DasTop())
    //    SpinalConfig().generateVerilog(DasTop())
  }
}
