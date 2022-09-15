package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SignalProWrapper(implicit staticConfig: DasStaticConfig)
  extends Component {

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
  clkOut := pll.outclks.head // 62.5MHz->125MHz

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
  }

  new ClockingArea(domainPro) {

    // declare cores working in signal processing time domain
    val controlRegs = ControlRegs()
    val selfTest = DasSelfTest()
    val signalPro = SignalPro()
    val pulseGen = PulseGen()

    /** --------
     * control signal allocation
     * -------- */
    controlRegs.ctrlIn := ctrlIn
    val regsOut = controlRegs.regsOut
    gainOut := regsOut.gain.resize(6 bits)

    pulseGen.pulsePeriodIn := regsOut.pulsePeriodFull.resized

    signalPro.flowIn.pulseChange := pulseGen.pulseChange
    signalPro.flowIn.modeChange := pulseGen.modeChange
    signalPro.flowIn.valid := True
    signalPro.gaugePointsIn := regsOut.gaugePoints.resized
    signalPro.gaugeReverseIn := regsOut.gaugeReverseFull
    signalPro.pulsePointsIn := regsOut.pulsePointsFull.resized

    signalPro.flowIn.index.assignDontCare()

    signalPro.flowIn.payload.head.assignFromBits(adc0X0S)
    signalPro.flowIn.payload.last.assignFromBits(adc0X1S)
    //    signalPro.XXX := regsOut.spatialPointsFull

    /** --------
     * mode change logic
     * -------- */
    val todoModeChange = RegInit(True)
    when(pulseGen.modeChange)(todoModeChange.clear())
    when(ctrlIn.ctrlUpdate)(todoModeChange.set())

    pulseGen.todoModeChange := todoModeChange

    /** --------
     * signal processing logic, running in domainPro
     * -------- */
    when(regsOut.mode === 0) { // self-testing mode
      selfTest.dataOut0 >> dataOut0
      selfTest.dataOut1 >> dataOut1
      pulseOutDefault()
      pulsesOut.Pulse_Single := pulseGen.pulseOut
    }.elsewhen(regsOut.mode === 1) { // raw mode, adc -> pcie
      dataOut0.payload := adc0X0S.resize(16) // first phase component of adc0
      dataOut1.payload := adc0X1S.resize(16) // first phase component of adc1
      dataOut0.valid := True
      dataOut1.valid := True
      pulseOutDefault()
      pulsesOut.Pulse_Single := pulseGen.pulseOut
      // 4KHz pulse out
    }.otherwise { // normal mode, do signal processing
      dataOut0.payload := signalPro.flowOut.payload.head.asBits.takeHigh(16)
      dataOut1.payload := dataOut1.payload.getZero
      dataOut0.valid := signalPro.flowOut.valid
      dataOut1.valid := False
      pulseOutDefault()
      pulsesOut.Pulse_Single := pulseGen.pulseOut
    }

    /** --------
     * fix signal names for SignalTap
     * -------- */
    clkOut.setName("clkOut")
    regsOut.mode.setName("mode")
    regsOut.gain.setName("gain")
    regsOut.gaugeReverseFull.setName("gaugeReverse")
    regsOut.gaugePoints.setName("gaugePoints")
    regsOut.pulsePointsFull.setName("pulsePoints")
    regsOut.pulsePeriodFull.setName("pulsePeriod")
    pulseGen.todoModeChange.setName("modeChange")
  }
}

