package org.datenlord
package ip.das

import spinal.core._
import scala.language.postfixOps

case class DasRegFileInterface() extends Bundle {
  val controlType = HardType(UInt(8 bits))
  val mode,
  pulsePoints0, pulsePoints1, pulsePoints2,
  gaugePoints,
  gaugeReverse0, gaugeReverse1, gaugeReverse2,
  spatialPoints0, spatialPoints1,
  pulsePeriod0, pulsePeriod1,
  position0, position1,
  gain = controlType()

  def pulsePointsFull: UInt = pulsePoints0 @@ pulsePoints1 @@ pulsePoints2

  def spatialPointsFull: UInt = spatialPoints0 @@ spatialPoints1

  def pulsePeriodFull: UInt = pulsePeriod0 @@ pulsePeriod1

  def positionFull: UInt = position0 @@ position1

  def gaugeReverseFull = {
    val ret = SFix(0 exp, -17 exp)
    val raw = (gaugeReverse0 @@ gaugeReverse1 @@ gaugeReverse2).resize(18)
    ret.assignFromBits(raw.asBits)
    ret
  }
}

// TODO: implement method "add register"

case class ControlRegs(implicit staticConfig: DasStaticConfig)
  extends Component {

  val constants = staticConfig.genConstants()
  import constants._

  val ctrlIn = in(CtrlInterface(xillybusDevices.find(_.name == "ctrl_8").get))
  val ctrlRegs = Reg(DasRegFileInterface())
  val regsOut = out(DasRegFileInterface())
  regsOut := ctrlRegs

  /** --------
   * regs initialization
   * -------- */
  val initConfig = DasRuntimeConfig(10.4, 9.9, 5e6, 5, 9000)
  val initRegValues = initConfig.genRegValues(staticConfig)

  logger.info(
    s"\n----initial state report----" +
      s"\n\t mode = 2" +
      s"\n\t pulsePoints = ${initRegValues.pulsePoints}" +
      s"\n\t pulsePeriod = ${initRegValues.pulsePeriod}" +
      s"\n\t gaugePoints = ${initRegValues.gaugePoints}" +
      s"\n\t spatialPoints = ${initRegValues.spatialPoints}" +
      s"\n\t position = ${initRegValues.position}" +
      s"\n\t gain = ${initRegValues.gain}"
  )

  // regs are in high to low order
  def initRegs(value: BigInt, regs: UInt*): Unit = {
    val length = regs.length * 8
    val bits = value.toString(2).padToLeft(length, '0')
    bits.grouped(8).toSeq.zip(regs).foreach { case (b, reg) => reg.init(U(b)) }
  }

  ctrlRegs.mode.init(2)
  initRegs(initRegValues.pulsePeriod, ctrlRegs.pulsePeriod0, ctrlRegs.pulsePeriod1)
  initRegs(initRegValues.pulsePoints / subFilterCount , ctrlRegs.pulsePoints0, ctrlRegs.pulsePoints1, ctrlRegs.pulsePoints2)
  initRegs(initRegValues.gaugePoints / subFilterCount, ctrlRegs.gaugePoints)
  initRegs(initRegValues.spatialPoints, ctrlRegs.spatialPoints0, ctrlRegs.spatialPoints1)
  initRegs(initRegValues.position, ctrlRegs.position0, ctrlRegs.position1)
  initRegs(initRegValues.gain, ctrlRegs.gain)

  val defaultGaugeReverse =
    SFConstant(1.0 / initRegValues.gaugePoints, HardType(SFix(0 exp, -17 exp))).asBits.asUInt
  ctrlRegs.gaugeReverse0.init(defaultGaugeReverse(17 downto 16).resize(8))
  ctrlRegs.gaugeReverse1.init(defaultGaugeReverse(15 downto 8))
  ctrlRegs.gaugeReverse2.init(defaultGaugeReverse(7 downto 0))

  /** --------
   * regs updating
   -------- */
  // TODO: do sync by an async FIFO in XillybusWrapper
  // TODO: divide by subFilterCount
  val ctrlUpdate = ctrlIn.ctrlUpdate.d(3)
  val ctrlAddr = ctrlIn.ctrlAddr.d(3)
  val ctrlValue = ctrlIn.ctrlValue.d(3)

  when(ctrlUpdate) { // delay for CDC
    switch(ctrlAddr) { // FIXME: metastability?
      is(0)(ctrlRegs.mode := ctrlValue)
      is(2)(ctrlRegs.pulsePoints0 := ctrlValue)
      is(3)(ctrlRegs.pulsePoints1 := ctrlValue)
      is(4)(ctrlRegs.pulsePoints2 := ctrlValue)
      is(5)(ctrlRegs.gaugePoints := ctrlValue)
      is(6)(ctrlRegs.spatialPoints0 := ctrlValue)
      is(7)(ctrlRegs.spatialPoints1 := ctrlValue)
      is(8)(ctrlRegs.pulsePeriod0 := ctrlValue)
      is(9)(ctrlRegs.pulsePeriod1 := ctrlValue)
      is(10)(ctrlRegs.gain := ctrlValue)
      is(12)(ctrlRegs.gaugeReverse0 := ctrlValue)
      is(13)(ctrlRegs.gaugeReverse1 := ctrlValue)
      is(14)(ctrlRegs.gaugeReverse2 := ctrlValue)
      is(16)(ctrlRegs.position0 := ctrlValue)
      is(17)(ctrlRegs.position1 := ctrlValue)
    }
  }
}