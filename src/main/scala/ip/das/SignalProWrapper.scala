package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SignalProWrapper(implicit staticConfig: DasStaticConfig)
  extends Component {

  val constants = staticConfig.genConstants()

  import constants._

  val clkIn, rstn = in Bool() // 62.5MHz
  val clkOut = out Bool() // 125MHz
  val adcData0, adcData1 = in Vec(Bits(14 bits), 4) // adc data in
  val ctrlIn = in(CtrlInterface(xillybusDevices.find(_.name == "ctrl_8").get))
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

  val domainAcq = ClockDomain(clock = clkIn, reset = rstn, config = dasClockConfig, frequency = FixedFrequency(62.5 MHz))
  val domainPro = ClockDomain(clock = clkOut, reset = rstn, config = dasClockConfig, frequency = FixedFrequency(125 MHz))

  /** --------
   * 62.5MHz -> 125MHz, parallel to serial, offset binary -> signed fix
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

  /** --------
   * signal processing under 125MHz
   * -------- */
  new ClockingArea(domainPro) {

    // offset binary -> signed fix, 8192 -> 0
    def formatChange(offsetBinary: Bits) = {
      val ret = adcDataType()
      val (msb, tail) = offsetBinary.splitAt(offsetBinary.getBitsWidth - 1)
      ret.assignFromBits(~msb ## tail)
      ret.d(1) // this "delay" happens in domainPro
    }

    // dataOut, S stands for serial
    val Seq(adc0X0S, adc0X1S, adc1X0S, adc1X1S) = p2ses.map(_.dataOut).map(formatChange)

    // declare cores working in signal processing time domain
    val controlRegs = ControlRegs()
    //    val selfTest = DasSelfTest()
    val signalPro = SignalPro()
    val spatialExtractor = SpatialExtractor()
    val pulseGen = PulseGen()
    val frameBuilder = FrameBuilder()

    /** --------
     * control signal allocation
     * -------- */
    // controlRegs module update registers according to ctrlIn
    controlRegs.ctrlIn := ctrlIn
    val regsOut = controlRegs.regsOut
    gainOut := regsOut.gain.resize(6 bits)

    // register values allocation
    // TODO: do resize in controlRegs module
    pulseGen.pulsePeriodIn := regsOut.pulsePeriodFull.resized

    signalPro.flowIn.pulseChange := pulseGen.pulseChange
    signalPro.flowIn.modeChange := pulseGen.modeChange
    signalPro.flowIn.valid := True
    signalPro.gaugePointsIn := regsOut.gaugePoints.resized
    signalPro.gaugeReverseIn := regsOut.gaugeReverseFull
    signalPro.pulsePointsIn := regsOut.pulsePointsFull.resized
    signalPro.spatialPointsIn := regsOut.spatialPointsFull.resized

    // signalPro -> extractor
    spatialExtractor.positionIn := regsOut.positionFull.resized
    spatialExtractor.flowIn := signalPro.flowOut
    // signalPro -> frameBuilder
    frameBuilder.flowIn := signalPro.flowOut

    /** --------
     * mode change logic
     * -------- */
    val todoModeChange = RegInit(True)
    pulseGen.todoModeChange := todoModeChange // ask the pulse generator to issue a modeChange pulse
    when(pulseGen.modeChange)(todoModeChange.clear()) // cleared after a modeChange pulse sent
    when(ctrlIn.ctrlUpdate)(todoModeChange.set()) // set after a parameter modification

    /** --------
     * user logic running in domainPro, do 1. demodulation 2. pulse generation
     * -------- */

    signalPro.flowIn.payload.head := adc0X0S
    signalPro.flowIn.payload.last := adc0X1S
    // this channel upload all demodulated results
    dataOut0.payload := frameBuilder.flowOut.payload.head.asBits.takeHigh(16)
    dataOut0.valid := frameBuilder.flowOut.valid
    // this channel upload demodulated result at a specific distance
    dataOut1.payload := spatialExtractor.flowOut.payload.head.asBits.takeHigh(16)
    dataOut1.valid := spatialExtractor.flowOut.valid

    signalPro.flowIn.index.assignDontCare() // for simulation only
    signalPro.flowIn.payload.assignDontCare() // pre-assignment
    pulsesOut := pulseGen.pulseOut
    //    when(regsOut.mode === 0) { // self-testing mode, counter -> pcie
    //      selfTest.dataOut0 >> dataOut0
    //      selfTest.dataOut1 >> dataOut1
    //    }.elsewhen(regsOut.mode === 1) { // raw mode, adc -> pcie
    //      dataOut0.payload := adc0X0S.asBits.resize(16) // first phase component of adc0
    //      dataOut1.payload := adc0X1S.asBits.resize(16) // second phase component of adc0
    //      dataOut0.valid := True
    //      dataOut1.valid := True
    //    }.otherwise { // normal mode, do signal processing, demodulated result -> pcie
    //
    //    }

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

