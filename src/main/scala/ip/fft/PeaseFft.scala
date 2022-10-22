package org.datenlord
package ip.fft

import arithmetic.{Diagonal, DiagonalConfig}
import arithmetic.Matrices.digitReversalPermutation
import flowConverters._
import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.numerics.ceil
import dsp.Dft._
import dsp.Dft
import org.datenlord.device.ComplexMult
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/**
 * by default, this module takes signed normalized input within [-1, 1) 256 8 128 1024
 */
case class PeaseFftConfig(N: Int, radix: Int,
                          dataWidth: Int, coeffWidth: Int,
                          inverse: Boolean, spaceReuse: Int, timeReuse: Int)
  extends TransformBase {

  // coefficients
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

  val iterativeCount = stageCount / timeReuse // number of iterative boxes in one iteration

  val totalShift = (n + 1) / 2
  val iterativeShift = ceil(totalShift.toDouble / timeReuse).toInt
  val iterativeBoxShift = iterativeShift.toDouble / iterativeCount
  val shifts = (0 until iterativeCount).map(i => ceil((i + 1) * iterativeBoxShift) - ceil(i * iterativeBoxShift)).map(_.toInt) // shifts after each iterative box
  require(shifts.sum == iterativeShift)
  val compensateShift = iterativeShift * timeReuse - totalShift

  def iterativeBoxLatency = {
    val permLatency = StridePermutationFor2Config(n, q, r, dataWidth * 2).latency
    multLatency + dftLatency + permLatency
  }

  def iterativeLatency = iterativeCount * iterativeBoxLatency

  def utilization = if (timeReuse == 1) 1 else if (spaceReuse > iterativeLatency) 1 else spaceReuse.toDouble / iterativeLatency

  def throughput = 1.0 / (spaceReuse * timeReuse) * utilization


  override val size = (N, N)

  override def latency = (spaceReuse max iterativeLatency) * timeReuse

  // TODO: define its flows by periodic flow
  override def inputFlow = TimeSpaceFlow(N, spaceReuse, timeReuse, iterativeLatency)

  override def outputFlow = TimeSpaceFlow(N, spaceReuse, timeReuse, iterativeLatency)

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[Complex]]
    val dftMatrix = Dft.dftMatrix(N, inverse)
    val input = new DenseVector(SpatialPermutation(data.toArray, bitReverse).toArray)
    val ret = if (!inverse) dftMatrix * input else dftMatrix * input / Complex(N, 0)
    val normalizeWidth = if (!inverse) (n + 1) / 2 else n / 2 // around sqrt(N), take upper for dft and lower for idft
    val normalizeValue = if (!inverse) (1 << normalizeWidth).toDouble else 1.0 / (1 << normalizeWidth)
    val normalized = ret / Complex(normalizeValue, 0) // for normalization
    normalized.toArray.toSeq
  }

  def dspEstimation = (N / spaceReuse) * (stageCount / timeReuse) * 3

  override def toString = s"generating pease fft on with " +
    s"spaceReuse = $spaceReuse, timeReuse = $timeReuse, dsp estimated = $dspEstimation, " +
    s"throughput inverse = ${1.0 / throughput}, utilization = $utilization, " +
    s"latency = $latency, iterative latency = $iterativeLatency"

  override def implH = PeaseFft(this)
}

case class PeaseFft(config: PeaseFftConfig) extends TransformModule[ComplexFix, ComplexFix] {

  import config._

  val dataType = HardType(ComplexFix(0 exp, -(dataWidth - 1) exp))
  val coeffType = HardType(ComplexFix(1 exp, -(coeffWidth - 2) exp))

  val innerMax = (n + 1) / 2 + 1
  val innerType = HardType(ComplexFix(innerMax exp, -(dataWidth - innerMax - 1) exp))

  override val dataIn = slave Flow Fragment(Vec(dataType, portWidth))
  override val dataOut = master Flow Fragment(Vec(innerType, portWidth))

  val iterLatencyCounter = CounterFreeRun(iterativeLatency max spaceReuse)
  when(dataIn.last)(iterLatencyCounter.clear())

  val iterCounter = Counter(timeReuse)
  when(dataIn.last)(iterCounter.clear())
  when(iterLatencyCounter.willOverflow)(iterCounter.increment())

  def iterativeBox(iterIn: Flow[Fragment[Vec[ComplexFix]]], iters: Seq[Int]) = {

    // multipliers
    val Cs = iters.map(l => diagC(N, l, radix, inverse))
    val coeffs = Cs.flatMap(_.diag.toArray)
    val cdmConfig: DiagonalConfig[Complex, ComplexFix] = DiagonalConfig(coeffs, innerType, coeffType, spaceReuse * timeReuse)
    val mults = Diagonal(cdmConfig)
    mults.dataIn << iterIn
    if (timeReuse > 1) {
      mults.dataIn.last.allowOverride
      mults.dataIn.last := dataIn.last
    }
    val afterMult = mults.dataOut.fragment.map(_.truncate(innerType().sfixType))

    // dfts
    val afterDft = Vec(afterMult.grouped(radix).toSeq.flatMap(seq => dft(Vec(seq)).map(_.truncate(innerType().sfixType))))

    // permutation
    val permConfig = StridePermutationFor2Config(n, q, r, innerType.getBitsWidth)
    val permutation = StridePermutationFor2(permConfig)
    val flowAfterDft = ChainsawFlow(Vec(afterDft.map(_.asBits)), iterIn.valid.validAfter(dftLatency + multLatency), iterIn.last.validAfter(dftLatency + multLatency))
    permutation.dataIn << flowAfterDft
    val afterPerm = cloneOf(afterDft)
    afterPerm.zip(permutation.dataOut.fragment).foreach { case (complex, bits) => complex.assignFromBits(bits) }

    ChainsawFlow(afterPerm, permutation.dataOut.valid, permutation.dataOut.last)
  }

  val dataInTruncated = dataIn.withFragment(dataIn.fragment.map(_.truncate(innerType().sfixType)))
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
    val normalized = next.fragment.map(_ >> shifts(i)).map(_.truncate(innerType().sfixType))
    dataPath += next.withFragment(normalized)
  }

  val padLatency = if (iterativeLatency < spaceReuse && timeReuse > 1) spaceReuse - iterativeLatency else 0

  val point = iterCounter.value === 0
  point.setName("fromDataIn")
  val iterEnd = dataPath.last.d(padLatency)
  if (timeReuse > 1) {
    iterStart.fragment := Mux(iterCounter.value === 0, dataInTruncated.fragment, iterEnd.fragment)
    iterStart.valid.assignDontCare()
    iterStart.last := iterLatencyCounter.willOverflow | dataIn.last
  }
  else iterStart << dataInTruncated

  dataOut.fragment := (if (timeReuse == 1) iterEnd.fragment else Vec(iterEnd.fragment.map(_ << compensateShift).map(_.truncate(innerType().sfixType))))
  autoValid()
  autoLast()
}
