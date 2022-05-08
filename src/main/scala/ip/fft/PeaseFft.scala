package org.datenlord
package ip.fft

import arithmetic.{ComplexDiagonalMatrix, ComplexDiagonalMatrixConfig}
import algos.Matrices.{digitReversalPermutation, stridePermutation}
import dataFlow._
import breeze.linalg.DenseVector
import breeze.math.Complex
import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/**
 * by default, this module takes signed normalized input within [-1, 1)
 */
case class PeaseFftConfig(N: Int, radix: Int,
                          dataWidth: Int, coeffWidth: Int,
                          inverse: Boolean, spaceReuse: Int, timeReuse: Int) extends TransformConfig {

  val n = logR(N, 2)
  val stageCount = logR(N, radix)
  val stageWidth = logR(radix, 2)

  require(spaceReuse <= N / radix && timeReuse <= stageCount)

  val bitReverse = digitReversalPermutation[Int](N, radix)
  val dft = BaseDft.radixRDft(radix)
  val dftLatency = BaseDft.latency(radix)
  val multLatency = ComplexMult.latency

  val portWidth = N / spaceReuse
  val q = logR(portWidth, 2)
  val r = logR(radix, 2)

  val iterativeCount = stageCount / timeReuse

  def iterativeBoxLatency = {
    val permLatency = StridePermutation2Config(n, q, r, dataWidth * 2).latency
    multLatency + dftLatency + permLatency
  }

  def iterativeLatency = iterativeCount * iterativeBoxLatency

  def utilization = if (timeReuse == 1) 1 else if (spaceReuse > iterativeLatency) 1 else spaceReuse.toDouble / iterativeLatency

  override def latency = (spaceReuse max iterativeLatency) * timeReuse

  override def inputFlow = TimeSpaceFlow(N, spaceReuse, timeReuse, iterativeLatency)

  override def outputFlow = TimeSpaceFlow(N, spaceReuse, timeReuse, iterativeLatency)

  override def complexTransform(dataIn: Seq[Complex]) = {
    val dftMatrix = algos.Dft.dftMatrix(N, inverse)
    val input = new DenseVector(SpatialPermutation(dataIn.toArray, bitReverse).toArray)
    val ret = if (!inverse) dftMatrix * input else dftMatrix * input / Complex(N, 0)
    val normalizeWidth = if (!inverse) (n + 1) / 2 else n / 2 // around sqrt(N), take upper for dft and lower for idft
    val normalizeValue = if (!inverse) (1 << normalizeWidth).toDouble else 1.0 / (1 << normalizeWidth)
    val normalized = ret / Complex(normalizeValue, 0) // for normalization
    normalized.toArray.toSeq
  }

  def dspEstimation = (N / spaceReuse) * (stageCount / timeReuse) * 3

  logger.info(s"generating pease fft on with " +
    s"spaceReuse = $spaceReuse, timeReuse = $timeReuse, dsp estimated = $dspEstimation, " +
    s"throughput = ${(1.0 / (spaceReuse * timeReuse) * utilization)}, utilization = ${utilization}, " +
    s"latency = $latency, iterative latency = $iterativeLatency")

}

case class PeaseFft(config: PeaseFftConfig) extends TransformModule[ComplexFix, ComplexFix] {

  import algos.Dft.diagC
  import config._

  val dataType = HardType(SFix(0 exp, -(dataWidth - 1) exp))
  val coeffType = HardType(SFix(1 exp, -(coeffWidth - 2) exp))

  val innerMax = (n + 1) / 2 + 3
  val innerType = HardType(SFix(innerMax exp, -(dataWidth - innerMax - 1) exp))

  override val dataIn = slave Flow Fragment(Vec(ComplexFix(dataType), portWidth))
  override val dataOut = master Flow Fragment(Vec(ComplexFix(innerType), portWidth))

  val iterLatencyCounter = CounterFreeRun(iterativeLatency max spaceReuse)
  when(dataIn.last)(iterLatencyCounter.clear())

  val iterCounter = Counter(timeReuse)
  when(dataIn.last)(iterCounter.clear())
  when(iterLatencyCounter.willOverflow)(iterCounter.increment())

  def iterativeBox(iterIn: Flow[Fragment[Vec[ComplexFix]]], iters: Seq[Int]) = {

    // multipliers
    val Cs = iters.map(l => diagC(N, l, radix, inverse))
    val coeffs = Cs.flatMap(_.diag.toArray)
    val cdmConfig = ComplexDiagonalMatrixConfig(coeffs, spaceReuse * timeReuse, innerType, coeffType)
    val mults = ComplexDiagonalMatrix(cdmConfig)
    mults.dataIn << iterIn
    if (timeReuse > 1) {
      mults.dataIn.last.allowOverride
      mults.dataIn.last := dataIn.last
    }
    val afterMult = mults.dataOut.fragment.map(_.truncated(innerType))

    // dfts
    val afterDft = Vec(afterMult.grouped(radix).toSeq.flatMap(seq => dft(Vec(seq)).map(_.truncated(innerType))))

    // permutation
    val permConfig = StridePermutation2Config(n, q, r, innerType.getBitsWidth * 2)
    val permutation = StridePermutation2(permConfig)
    val flowAfterDft = ChainsawFlow(Vec(afterDft.map(_.asBits)), iterIn.valid.validAfter(dftLatency + multLatency), iterIn.last.validAfter(dftLatency + multLatency))
    permutation.dataIn << flowAfterDft
    val afterPerm = cloneOf(afterDft)
    afterPerm.zip(permutation.dataOut.fragment).foreach { case (complex, bits) => complex.assignFromBits(bits) }

    ChainsawFlow(afterPerm, permutation.dataOut.valid, permutation.dataOut.last)
  }

  val dataInTruncated = dataIn.withFragment(dataIn.fragment.map(_.truncated(innerType)))
  val iterStart = cloneOf(dataInTruncated)

  val dataPath = ArrayBuffer[Flow[Fragment[Vec[ComplexFix]]]](iterStart)

  // in our implementation, 1/N is not implemented in DFTs
  // ifft should be shift right n (for 1/N) and then shifted left n/2 (for normalization)
  // as a result, it should be shifted right (n+1)/2, same as fft
  def normalizedWidthOnStage(stage: Int) = {
    if (stage == 0) 0
    else (stage * stageWidth + 1) / 2
  }

  (0 until iterativeCount).reverse.map { i =>
    val coeffSet = (0 until timeReuse).map(j => j * iterativeCount + i).reverse
    val next = iterativeBox(dataPath.last, coeffSet)
    val normalizeWidth = if(timeReuse == 1) normalizedWidthOnStage(i + 1) - normalizedWidthOnStage(i) else 0
    println(s"norm on stage $normalizeWidth")
    val normalized = next.fragment.map(_ >> normalizeWidth).map(_.truncated(innerType))
    dataPath += next.withFragment(normalized)
  }


  val padLatency = if (iterativeLatency < spaceReuse && timeReuse > 1) spaceReuse - iterativeLatency else 0
  println(s"pad $padLatency")

  val point = iterCounter.value === 0
  point.setName("fromDataIn")
  val iterEnd = dataPath.last.d(padLatency)
  if (timeReuse > 1) {
    iterStart.fragment := Mux(iterCounter.value === 0, dataInTruncated.fragment, iterEnd.fragment)
    iterStart.valid.assignDontCare()
    iterStart.last := iterLatencyCounter.willOverflow | dataIn.last
  }
  else iterStart << dataInTruncated

  dataOut.fragment := (if(timeReuse == 1) iterEnd.fragment else Vec(iterEnd.fragment.map(_ >> (n + 1) / 2).map(_.truncated(innerType))))
  autoValid()
  autoLast()
}
