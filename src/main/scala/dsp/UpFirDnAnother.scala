package org.datenlord
package dsp

import dsp.FilterStructure.SYSTOLIC

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class UpFirDnAnotherConfig(upSample: Int, multiple: Int,
                                coeffs: Seq[Double], typeIn: HardType[SFix])
  extends TransformBase {

  val taps = coeffs.length
  val typeCoeff = HardType(SFix(0 exp, -17 exp))
  val typeOut = HardType(SFix(log2Up(taps) exp, -11 exp))

  /** --------
   * taps calculation
   * -------- */
  val phaseCount = multiple * upSample
  require(coeffs.length % phaseCount == 0)
  val subFilterTaps = coeffs.length / phaseCount
  val subFilterLatency = DoFir.latency(subFilterTaps, SYSTOLIC)

  // poly phase decomposition of coefficients
  val coeffGroups = (0 until phaseCount).map(i => coeffs.zipWithIndex.filter(_._2 % phaseCount == i).map(_._1))

  /** --------
   * poly phase network construction
   * -------- */
  case class TermZ(xIndex: Int, hIndex: Int, z: Int) {
    def order = xIndex + hIndex

    def delay(value: Int) = TermZ(xIndex, hIndex, z - value)

    def *(that: TermZ) = TermZ(xIndex + that.xIndex, hIndex + that.hIndex, z + that.z)

    override def toString = s"z${z}X${xIndex}H$hIndex"
  }

  val polyX = (0 until multiple).map(i => TermZ(i * upSample, 0, -i * upSample))
  val polyH = (0 until phaseCount).map(i => TermZ(0, i, -i))
  // polynomial multiplication
  val allTerms = Seq.tabulate(multiple, phaseCount) { (i, j) =>polyX(i) * polyH(j)}.flatten
  //  find out y(n) components
  val termYs = (0 until phaseCount)
    .map(i => allTerms.filter(_.order % phaseCount == i).map(_.delay(-i)))
  termYs.zipWithIndex.foreach { case (terms, i) => logger.info(s"Y$i = ${terms.mkString(" + ")}") }
  val sumLatency = log2Up(termYs.head.length)

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[Double]]
    matlab.Dsp.upfirdn(data.toArray, coeffs.toArray, upSample, 1)
      .drop((subFilterTaps - 1) * phaseCount).dropRight(phaseCount - 1)
  }

  override val implMode = Infinite

  override val size = (multiple, multiple * upSample)

  override def latency = subFilterLatency + sumLatency

  override def implH = UpFirDnAnother(this)
}

case class UpFirDnAnother(config: UpFirDnAnotherConfig) extends TransformModule[SFix, SFix] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(typeIn(), inputWidth))
  override val dataOut = master Flow Fragment(Vec(typeOut(), outputWidth))

  val coeffsGroupHard = coeffGroups.map(_.map(SFConstant(_, typeCoeff())))

  /** --------
   * hardware construction
   * -------- */
  def mult(data: SFix, coeff: SFix) = (data * coeff).truncate(typeOut)

  def add(a: SFix, b: SFix) = a + b

  def pipeline(value: SFix, nothing: Int) = value.d(1)

  val rets = termYs.map { terms =>
    val subFilterRets = terms.map { term =>
      val inPort = dataIn.payload(term.xIndex / upSample)
      val coeffs = coeffsGroupHard(term.hIndex)
      val delay = -(term.z / outputWidth) // divided by sizeOut as the system is running at a higher speed
      DoFir(inPort, coeffs, mult, add, SYSTOLIC).d(delay)
    }
    subFilterRets.reduceBalancedTree(add, pipeline)
  }

  dataOut.fragment.zip(rets).foreach { case (port, value) => port := value }
  autoValid()
  autoLast()
}