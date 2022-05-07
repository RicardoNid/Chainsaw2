package org.datenlord

import dataFlow._
import spinal.core._

/** configuration of complex multiplication
 *
 * @param fast     when set, use 3 DSPs(rather than 4)
 * @param pipeline number of pipelining stages, i.e. latency
 */
case class ComplexMultConfig(fast: Boolean = true, pipeline: Int = 6)

case class ComplexMult(dataType: HardType[ComplexFix], coeffType: HardType[ComplexFix])
  extends Component {

  val a = in(dataType())
  val b = in(coeffType())
  val (ar, ai) = (a.real, a.imag)
  val (br, bi) = (b.real, b.imag)
  val retPeak = a.peak.value + b.peak.value + 2
  val retResolution = a.resolution.value + b.resolution.value
  val p = out(ComplexFix(retPeak exp, retResolution exp))

  // regs outside dsp
  val arD1 = ar.d(1)
  val aiD2 = ai.d(2)
  val brD2 = br.d(2)
  val biD2 = bi.d(2)
  // dsp operation and regs inside dsp
  val mid = ((br.d(1) +^ bi.d(1)).d(1) * ar.d(2)).d(2)
  p.imag := (mid.d(1) + ((aiD2.d(1) -^ arD1.d(2)).d(1) * brD2.d(2)).d(1)).d(1)
  p.real := (mid.d(1) - ((aiD2.d(1) +^ arD1.d(2)).d(1) * biD2.d(2)).d(1)).d(1)

}

