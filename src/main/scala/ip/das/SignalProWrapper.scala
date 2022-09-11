package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SignalProWrapper() extends Component {

  val clkIn, rstn = in Bool() // 62.5MHz
  val clkOut = out Bool() // 125MHz
  val adcData0, adcData1 = in Vec(Bits(14 bits), 4) // adc data in
  val ctrlIn = in(CtrlInterface(DasXillybusDevices.ctrl))
  val dataOut0, dataOut1 = master Flow Bits(16 bits)
  val pulsesOut = PulsesOut()
  val gainOut = out UInt (6 bits)

  /** --------
   * clock generation
   * -------- */
  val pll = AlteraPll(1)
  pll.setDefinitionName("P2SPLL")
  pll.refclk := clkIn
  pll.rst := ~rstn
  clkOut := pll.outclks.head // 62.5MHz, 125MHz

  val domainPro = ClockDomain(clock = clkOut, reset = rstn, config = dasClockConfig, frequency = FixedFrequency(125 MHz))

  /** --------
   * 62.5MHz -> 125MHz, parallel to serial
   * -------- */
  // clk & reset connection
  val p2ses = Seq.fill(4)(P2SCC(14, 2, 62.5 MHz))
  p2ses.foreach { p2s =>
    p2s.clkIn := clkIn
    p2s.rstn := rstn
    p2s.clkOut := clkOut
  }
  // data connections
  val Seq(p2s00, p2s01, p2s10, p2s11) = p2ses
  // construct parallel dataIn by poly phase decomposition, P stands for parallel
  val adc0X0P = Vec(adcData0(0), adcData0(2))
  val adc0X1P = Vec(adcData0(1), adcData0(3))
  val adc1X0P = Vec(adcData1(0), adcData1(2))
  val adc1X1P = Vec(adcData1(1), adcData1(3))
  // dataIn
  p2s00.dataIns := adc0X0P
  p2s01.dataIns := adc0X1P
  p2s10.dataIns := adc1X0P
  p2s11.dataIns := adc1X1P
  // dataOut, S stands for serial
  val Seq(adc0X0S, adc0X1S, adc1X0S, adc1X1S) = p2ses.map(_.dataOut)

  def pulseOutDefault(): Unit = {
    pulsesOut.Pulseout0 := clkIn
    pulsesOut.Pulseout0N := False
    pulsesOut.Pulseout1 := clkIn
    pulsesOut.Pulseout1N := False
    pulsesOut.Pulse_Single := clkIn
  }

  new ClockingArea(domainPro) {

    /** --------
     * register file for control
     * -------- */
    val controlType = HardType(UInt(8 bits))
    val mode, gain = Reg(controlType)
    // initialization
    mode.init(0) // raw mode
    gain.init(0) // minimum gain
    // control update logic
    // TODO: do sync by an async FIFO in XillybusWrapper
    when(ctrlIn.ctrlUpdate.d(3)) { // delay for CDC
      switch(ctrlIn.ctrlAddr.d(3)) { // FIXME: metastability?
        is(0)(mode := ctrlIn.ctrlValue.d(3))
        is(31)(gain := ctrlIn.ctrlValue.d(3))
      }
    }
    gainOut := gain.resize(6 bits)

    val selfTestCore = DasSelfTest()

    // TODO: implement signal processing logic in a module like DasSelfTest
    /** --------
     * signal processing logic, running in domainPro
     * -------- */
    when(mode === 0) { // self-testing mode
      selfTestCore.dataOut0 >> dataOut0
      selfTestCore.dataOut1 >> dataOut1
      pulsesOut := selfTestCore.pulsesOut
    }.elsewhen(mode === 1) { // raw mode, adc -> pcie
      dataOut0.payload := adc0X0S.resize(16) // first phase component of adc0
      dataOut1.payload := adc1X0S.resize(16) // first phase component of adc1
      dataOut0.valid := True
      dataOut1.valid := True
      pulsesOut := selfTestCore.pulsesOut
    }.otherwise { // normal mode
      dataOut0.payload := adc0X0S.resize(16)
      dataOut1.payload := adc1X0S.resize(16)
      dataOut0.valid := True
      dataOut1.valid := True
      pulseOutDefault()
    }
  }
}

