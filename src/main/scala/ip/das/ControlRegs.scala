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
  gain = controlType()

  def pulsePointsFull: UInt = pulsePoints0 @@ pulsePoints1 @@ pulsePoints2

  def spatialPointsFull: UInt = spatialPoints0 @@ spatialPoints1

  def pulsePeriodFull: UInt = pulsePeriod0 @@ pulsePeriod1

  def gaugeReverseFull = {
    val ret = SFix(0 exp, -17 exp)
    val raw = (gaugeReverse0 @@ gaugeReverse1 @@ gaugeReverse2).resize(18)
    ret.assignFromBits(raw.asBits)
    ret
  }
}

case class ControlRegs(implicit staticConfig: DasStaticConfig)
  extends Component {

  val ctrlIn = in(CtrlInterface(DasXillybusDevices.ctrl))
  val ctrlRegs = Reg(DasRegFileInterface())
  val regsOut = out(DasRegFileInterface())
  regsOut := ctrlRegs

  val initConfig = DasRuntimeConfig(10.4, 9.9, 5e6, 25)
  val initRegValues = initConfig.genRegValues(staticConfig)

  ctrlRegs.mode.init(2)
  ctrlRegs.gain.init(5)

  ctrlRegs.pulsePeriod0.init(initRegValues.pulsePeriod / 256)
  ctrlRegs.pulsePeriod1.init(initRegValues.pulsePeriod % 256)

  ctrlRegs.pulsePoints0.init(initRegValues.pulsePoints / 256 / 256)
  ctrlRegs.pulsePoints1.init((initRegValues.pulsePoints / 256) % 256)
  ctrlRegs.pulsePoints2.init(initRegValues.pulsePoints % 256)

  ctrlRegs.gaugePoints.init(initRegValues.gaugePoints)

  ctrlRegs.spatialPoints0.init(initRegValues.spatialPoints / 256)
  ctrlRegs.spatialPoints1.init(initRegValues.spatialPoints % 256)

  val defaultGaugeReverse =
    SFConstant(1.0 / initRegValues.gaugePoints, HardType(SFix(0 exp, -17 exp))).asBits.asUInt

  ctrlRegs.gaugeReverse0.init(defaultGaugeReverse(17 downto 16).resize(8))
  ctrlRegs.gaugeReverse1.init(defaultGaugeReverse(15 downto 8))
  ctrlRegs.gaugeReverse2.init(defaultGaugeReverse(7 downto 0))

  logger.warn(s"period = ${initRegValues.pulsePeriod}")

  // TODO: do sync by an async FIFO in XillybusWrapper
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
    }
  }
}