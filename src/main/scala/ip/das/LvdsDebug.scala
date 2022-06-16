package org.datenlord
package ip.das

import spinal.core._

case class LvdsDebug(desFactor: Int) extends Component {

  require(desFactor == 4)
  // I/O assigned
  // ADC related
  val adc_clk = in Bool() // 250MHz
  val rstn = in Bool()
  val signalAcqInterface = SignalAcqInterface(desFactor)
  signalAcqInterface.setName("")

  // 时钟生成
  val lvdsPll = AlteraPll(3)
  lvdsPll.setDefinitionName("LVDSPLL")
  lvdsPll.refclk := adc_clk
  lvdsPll.rst := ~rstn
  val Seq(clkSerial, // 500MHz,输入采样时钟
  clkEnable, // 62.5MHz,lvds enable时钟
  clkParallel // 62.5MHz,输出采样时钟
  ) = lvdsPll.outclk

  //  val fifoPll = AlteraPll(1)
  //  fifoPll.setName("FIFOPLL")
  //  fifoPll.refclk := clkParallel
  //  fifoPll.rst := ~rstn
  //  val clkBase = fifoPll.outclk.head // 250MHz,信号处理时钟

  val signalAcq = SignalAcq(desFactor)
  signalAcqInterface <> signalAcq.signalAcqInterface
  signalAcq.rstn := rstn
  signalAcq.clkSerial := clkSerial
  signalAcq.clkEnable := clkEnable
  signalAcq.clkParallel := clkParallel

  // 控制输入的可调放大器,
  val VGAMAX = 0
  val VGAMIN = 63
  val vga_b = out UInt (6 bits) // control the amplitude of the amplifier
  vga_b := U(VGAMIN, 6 bits)
}

object LvdsDebug {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "/home/ltr/sysudas/project/LvdsDebug").generateVerilog(LvdsDebug(4))
  }
}
