package org.datenlord
package dsp

import dsp.FilterStructure._

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random
// TODO: implement decimation
// TODO: implement this as a Chainsaw Transform
// TODO: adjustable precision
case class UpFirDn(up: Int, multiple: Int, coeffs: Seq[Double])
  extends Filter {

  override val sizeIn = multiple
  override val sizeOut = multiple * up
  override val typeIn = HardType(SFix(0 exp, -13 exp))
  override val typeCoeff = HardType(SFix(0 exp, -17 exp))
  override val typeOut = HardType(SFix(log2Up(taps) exp, -11 exp))
  override val dataIn = getDataIn
  override val dataOut = getDataOut

  val phaseCount = multiple * up
  require(coeffs.length % phaseCount == 0)
  val subFilterTaps = coeffs.length / phaseCount
  val subFilterLatency = DoFir.latency(subFilterTaps, SYSTOLIC)

  override def impl: Seq[Double] => Seq[Double] = (dataIn: Seq[Double]) =>
    matlab.Dsp.upfirdn(dataIn.toArray, coeffs.toArray, up, 1)
      .drop((subFilterTaps - 1) * phaseCount).dropRight(phaseCount - 1)

  // poly phase decomposition of coefficients
  val coeffGroups = (0 until phaseCount).map(i => coeffsHard.zipWithIndex.filter(_._2 % phaseCount == i).map(_._1))

  case class TermZ(xIndex: Int, hIndex: Int, z: Int) {
    def order = xIndex + hIndex

    def delay(value: Int) = TermZ(xIndex, hIndex, z - value)

    override def toString = s"z${z}X${xIndex}H$hIndex"
  }

  /** --------
   * poly phase network construction
   * -------- */
  // decomposition of x(n) and h(n)
  val xFactor = phaseCount / multiple
  val polyX = (0 until multiple).map(i => TermZ(i * xFactor, 0, -i * xFactor))
  val polyH = (0 until phaseCount).map(i => TermZ(0, i, -i))
  // polynomial multiplication
  val allTerms = Seq.tabulate(multiple, phaseCount) {
    (i, j) =>
      val termX = polyX(i)
      val termH = polyH(j)
      TermZ(termX.xIndex, termH.hIndex, termX.z + termH.z)
  }.flatten
  //  find out y(n) components
  val termYs = (0 until phaseCount)
    .map(i => allTerms.filter(_.order % phaseCount == i).map(_.delay(-i)))
  termYs.zipWithIndex.foreach { case (terms, i) => logger.info(s"Y$i = ${terms.mkString(" + ")}") }
  val sumLatency = log2Up(termYs.head.length)
  val padLatency = -termYs.map(_.map(_.z).min).min

  /** --------
   * hardware construction
   * -------- */
  // operators
  def mult(data: SFix, coeff: SFix) = (data * coeff).truncate(typeOut)

  def add(a: SFix, b: SFix) = a + b

  def pipeline(value: SFix, nothing: Int) = value.d(1)

  val rets = termYs.map { terms =>
    val subFilterRets = terms.map { term =>
      val inPort = dataIn.payload(term.xIndex / xFactor)
      val coeffs = coeffGroups(term.hIndex)
      val delay = -(term.z / sizeOut) // divided by sizeOut as the system is running at a higher speed
      DoFir(inPort, coeffs, mult, add, SYSTOLIC).d(delay)
    }
    subFilterRets.reduceBalancedTree(_ + _, pipeline)
  }

  override def latency = subFilterLatency + sumLatency

  logger.info(s"latency = $latency")

  dataOut.payload.zip(rets).foreach { case (port, value) => port := value }
  dataOut.valid := dataIn.valid.validAfter(latency)
}

object UpFirDn extends App {

  val data = (0 until 100).map(_ => Random.nextDouble())
  val coeffs = (0 until 144).map(_ => Random.nextDouble())
  FilterTest.test(UpFirDn(8, 2, coeffs), data)

}