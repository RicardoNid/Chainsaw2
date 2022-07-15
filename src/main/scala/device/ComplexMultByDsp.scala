package org.datenlord
package device

import spinal.core._

/** configuration of complex multiplication
 *
 * @param fast     when set, use 3 DSPs(rather than 4)
 * @param pipeline number of pipelining stages, i.e. latency
 */
case class ComplexMultConfig(fast: Boolean = true, pipeline: Int = 6)

case class ComplexMult(dataType: HardType[SFix], coeffType: HardType[SFix])
  extends Component {

  val data = in(ComplexFix(dataType()))
  val coeff = in(ComplexFix(coeffType()))
  val (ar, ai) = (data.real, data.imag)
  val (br, bi) = (coeff.real, coeff.imag)
  val retPeak = data.peak.value + coeff.peak.value + 2
  val retResolution = data.resolution.value + coeff.resolution.value
  val product = out(ComplexFix(retPeak exp, retResolution exp))

  // regs outside dsp
  val arD1 = ar.d(1)
  val aiD2 = ai.d(2)
  val brD2 = br.d(2)
  val biD2 = bi.d(2)
  // dsp operation and regs inside dsp
  val mid = ((br.d(1) +^ bi.d(1)).d(1) * ar.d(2)).d(2)
  product.imag := (mid.d(1) + ((aiD2.d(1) -^ arD1.d(2)).d(1) * brD2.d(2)).d(1)).d(1)
  product.real := (mid.d(1) - ((aiD2.d(1) +^ arD1.d(2)).d(1) * biD2.d(2)).d(1)).d(1)
}

object ComplexMult {

  def complexMult(data:ComplexFix, coeff:ComplexFix) = {
    val core = ComplexMult(data.sfixType, coeff.sfixType)
    core.data := data
    core.coeff := coeff
    core.product
  }

  def latency = 6

  def main(args: Array[String]): Unit = {
    val precision = HardType(SFix(2 exp, -13 exp))
    VivadoSynth(ComplexMult(precision, precision))
  }

}
