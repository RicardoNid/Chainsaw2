package org.datenlord
package ip.das

import org.datenlord.intel.{AlteraLvdsRx, AlteraPll}
import spinal.core._
import spinal.lib._

case class AdcInterface() extends Bundle {
  // ADC输出,采用DDR时序,在上升/下降沿的一对7位数据组成一个14位输出
  val adc_in_a, adc_in_b = in UInt (7 bits)
}

case class SignalAcq() extends Component {

  // I/O assigned
  val clkRef = in Bool()
  val rstn = in Bool()
  val adcInterface = AdcInterface()
  val clkOut = out Bool()

  // a,b来自不同的ADC通道,序号0~3是解串后的不同分量
  val dout_a, dout_b = out Vec(UInt(14 bits), 4)

  // 时钟生成
  val lvdsPll = AlteraPll(3)
  lvdsPll.setDefinitionName("LVDSPLL")
  lvdsPll.refclk := clkRef
  lvdsPll.rst := ~rstn
  val Seq(clkSerial, // 500MHz,输入采样时钟
  clkEnable, // 62.5MHz,lvds enable时钟
  clkParallel // 62.5MHz,输出采样时钟
  ) = lvdsPll.outclk
  clkOut := clkParallel

  val lvdsClockDomain = ClockDomain(
    clock = clkParallel, reset = rstn, config = dasClockConfig,
    frequency = FixedFrequency(250.0 / 4 MHz)
  )

  // lvds
  // as the ADC output is in DDR format, factor here is actual desFactor multiplied by 2
  val lvdsA, lvdsB = AlteraLvdsRx(7, 4 * 2)
  lvdsA.setDefinitionName("LVDS14")
  lvdsB.setDefinitionName("LVDS14")

  new ClockingArea(lvdsClockDomain) {

    import adcInterface.{adc_in_a,adc_in_b}
    // lvds connections
    Seq(lvdsA, lvdsB).zip(Seq(adc_in_a, adc_in_b).zip(Seq(dout_a, dout_b))).foreach {
      case (lvds, (dataIn, dataOut)) =>
        lvds.rx_inclock := clkSerial
        lvds.rx_enable := clkEnable
        lvds.rx_in := dataIn.asBits
        // 从lvds的输出中按提取数据,使得每个14bit的数据是一个ADC输出的无符号测量值
        dataOut.zipWithIndex.map { case (port, i) =>
          val out1of4prev = lvds.rx_out.asBools.zipWithIndex.filter(_._2 % (4 * 2) == i * 2).map(_._1) // e.g. 48,40,32...
          val out1of4next = lvds.rx_out.asBools.zipWithIndex.filter(_._2 % (4 * 2) == i * 2 + 1).map(_._1) // e.g. 49,41,33...
          val combined = out1of4next.zip(out1of4prev).flatMap { case (next, prev) => Seq(next, prev) }.asBits() // e.g. 48,49,40,41...
          port := combined.d(1).asUInt
        }
    }
  }
}