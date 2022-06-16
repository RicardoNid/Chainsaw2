package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

case class SignalAcqInterface(desFactor: Int) extends Bundle {
  // ADC输出,采用DDR时序,在上升/下降沿的一对7位数据组成一个14位输出
  val adc_in_a, adc_in_b = in UInt (7 bits) // data from two ADCs
  // a,b来自不同的ADC通道,序号0~3是解串后的不同分量
  val dout_a, dout_b = out Vec(UInt(14 bits), desFactor) // LVDS signal out for user logic
}

case class SignalAcq(desFactor: Int) extends Component {

  // I/O assigned
  val clkSerial, clkEnable, clkParallel = in Bool() // 62.5MHz
  val rstn = in Bool()
  val signalAcqInterface = SignalAcqInterface(desFactor)
  val clockConfig = ClockDomainConfig(resetActiveLevel = LOW)
  val lvdsClockDomain = ClockDomain(
    clock = clkParallel, reset = rstn, config = clockConfig,
    frequency = FixedFrequency(250.0 / desFactor MHz)
  )

  // lvds
  // as the ADC output is in DDR format, factor here is actual desFactor multiplied by 2
  val lvdsA, lvdsB = AltLvdsRx(7, desFactor * 2)
  lvdsA.setDefinitionName("LVDS14")
  lvdsB.setDefinitionName("LVDS14")

  new ClockingArea(lvdsClockDomain) {

    import signalAcqInterface.{adc_in_a,adc_in_b,dout_a,dout_b}
    // lvds connections
    Seq(lvdsA, lvdsB).zip(Seq(adc_in_a, adc_in_b).zip(Seq(dout_a, dout_b))).foreach {
      case (lvds, (dataIn, dataOut)) =>
        lvds.rx_inclock := clkSerial
        lvds.rx_enable := clkEnable
        lvds.rx_in := dataIn.asBits
        // 从lvds的输出中按提取数据,使得每个14bit的数据是一个ADC输出的无符号测量值
        dataOut.zipWithIndex.map { case (port, i) =>
          val out1of4prev = lvds.rx_out.asBools.zipWithIndex.filter(_._2 % (desFactor * 2) == i * 2).map(_._1) // e.g. 48,40,32...
          val out1of4next = lvds.rx_out.asBools.zipWithIndex.filter(_._2 % (desFactor * 2) == i * 2 + 1).map(_._1) // e.g. 49,41,33...
          val combined = out1of4next.zip(out1of4prev).flatMap { case (next, prev) => Seq(next, prev) }.asBits() // e.g. 48,49,40,41...
          port := combined.d(1).asUInt
        }
    }
  }
}

object SignalAcq {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateVerilog(SignalAcq(1))
  }
}
