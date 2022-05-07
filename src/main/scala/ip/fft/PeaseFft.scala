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
                          inverse: Boolean, fold: Int) extends TransformConfig {

  val n = logR(N, 2)
  val stageCount = logR(N, radix)
  val stageWidth = logR(radix, 2)

  val bitReverse = digitReversalPermutation[Int](N, radix)
  val dft = BaseDft.radixRDft(radix)
  val dftLatency = BaseDft.latency(radix)

  val portWidth = N / fold
  val q = logR(portWidth, 2)
  val r = logR(radix, 2)

  def iterativeLatency = {
    val multLatency = ComplexMult.latency
    val dftLatency = BaseDft.latency(radix)
    val permLatency = StridePermutation2Config(n, q, r, dataWidth * 2).latency
    multLatency + dftLatency + permLatency
  }

  override def latency = stageCount * iterativeLatency

  override def inputFlow = CyclicFlow(portWidth, fold)

  override def outputFlow = CyclicFlow(portWidth, fold)

  override def complexTransform(dataIn: Seq[Complex]) = {
    val dftMatrix = algos.Dft.dftMatrix(N, inverse)
    val input = new DenseVector(SpatialPermutation(dataIn.toArray, bitReverse).toArray)
    val ret = if (!inverse) dftMatrix * input else dftMatrix * input / Complex(N, 0)
    val normalizeWidth = if (!inverse) (n + 1) / 2 else n / 2 // around sqrt(N), take upper for dft and lower for idft
    val normalizeValue = if (!inverse) (1 << normalizeWidth).toDouble else 1.0 / (1 << normalizeWidth)
    val normalized = ret / Complex(normalizeValue, 0) // for normalization
    normalized.toArray.toSeq
  }
}

case class PeaseFft(config: PeaseFftConfig) extends TransformModule[ComplexFix, ComplexFix] {

  import algos.Dft.diagC
  import config._

  val dataType = HardType(SFix(0 exp, -(dataWidth - 1) exp))
  val coeffType = HardType(SFix(1 exp, -(coeffWidth - 2) exp))

  val innerMax = (n + 1) / 2
  val innerType = HardType(SFix(innerMax exp, -(dataWidth - innerMax - 1) exp))

  override val dataIn = slave Flow Fragment(Vec(ComplexFix(dataType), portWidth))
  override val dataOut = master Flow Fragment(Vec(ComplexFix(innerType), portWidth))

  def iterativeBox(dataIn: Seq[ComplexFix], l: Int, valid: Bool, last: Bool) = {

    // multipliers
    val C = diagC(N, l, radix, inverse)
    val cdmConfig = ComplexDiagonalMatrixConfig(C.diag.toArray.toSeq, fold, innerType, coeffType)
    val mults = ComplexDiagonalMatrix(cdmConfig)
    mults.dataIn.valid := valid
    mults.dataIn.last := last
    mults.dataIn.fragment := Vec(dataIn.map(_.truncated(innerType)))
    val afterMult = mults.dataOut.fragment.map(_.truncated(innerType))

    // dfts
    val afterDft = Vec(afterMult.grouped(radix).toSeq.flatMap(seq => dft(Vec(seq)).map(_.truncated(innerType))))

    // permutation
    val permConfig = StridePermutation2Config(n, q, r, innerType.getBitsWidth * 2)
    val permutation = StridePermutation2(permConfig)
    permutation.dataIn.fragment := Vec(afterDft.map(_.asBits))
    permutation.dataIn.valid := mults.dataOut.valid.validAfter(dftLatency)
    permutation.dataIn.last := mults.dataOut.last.validAfter(dftLatency)
    val afterPerm = cloneOf(afterDft)
    afterPerm.zip(permutation.dataOut.fragment).foreach { case (complex, bits) => complex.assignFromBits(bits) }

    (afterPerm, permutation.dataOut.valid, permutation.dataOut.last)
  }

  val dataPath = ArrayBuffer[Seq[ComplexFix]](dataIn.fragment)
  val validPath = ArrayBuffer[Bool](dataIn.valid)
  val lastPath = ArrayBuffer[Bool](dataIn.last)

  // in our implementation, 1/N is not implemented in DFTs
  // ifft should be shift right n (for 1/N) and then shifted left n/2 (for normalization)
  // as a result, it should be shifted right (n+1)/2, same as fft
  def normalizedWidthOnStage(stage: Int) = {
    if (stage == 0) 0
    else (stage * stageWidth + 1) / 2
  }

  (0 until stageCount).reverse.map { i =>
    val next = iterativeBox(dataPath.last, i, validPath.last, lastPath.last)
    val normalizeWidth = normalizedWidthOnStage(i + 1) - normalizedWidthOnStage(i)
    val normalized = next._1.map(_ >> normalizeWidth).map(_.truncated(innerType))
    //    println(s"normalized by $normalizeWidth")
    dataPath += normalized
    validPath += next._2
    lastPath += next._3
  }

  dataOut.fragment := Vec(dataPath.last)
  autoValid()
  autoLast()
}
