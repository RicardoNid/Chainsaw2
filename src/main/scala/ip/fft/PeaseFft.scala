package org.datenlord
package ip.fft

import dataFlow._

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.numerics.sqrt
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/**
 * by default, this module takes signed normalized input within [-1, 1)
 */
case class PeaseFftConfig(N: Int, radix: Int, dataWidth: Int, coeffWidth: Int, inverse: Boolean, fold: Int) extends TransformConfig {

  val n = logR(N, 2)
  val stageCount = logR(N, radix)
  val stageWidth = logR(radix, 2)

  override def latency = stageCount * (6 + BaseDft.latency(radix))

  override def inputFlow = FullyPipelinedFlow(N)

  override def outputFlow = FullyPipelinedFlow(N)

  override def complexTransform(dataIn: Seq[Complex]) = {
    val dftMatrix = algos.Dft.dftMatrix(N, inverse)
    val ret = if (!inverse) dftMatrix * new DenseVector(dataIn.toArray) else dftMatrix * new DenseVector(dataIn.toArray) / Complex(N, 0)
    val normalizeWidth = if (!inverse) (n + 1) / 2 else n / 2 // around sqrt(N), take upper for dft and lower for idft
    val normalizeValue = if (!inverse) (1 << normalizeWidth).toDouble else 1.0 / (1 << normalizeWidth)
    val normalized = ret / Complex(normalizeValue, 0) // for normalization
    normalized.toArray.toSeq
  }
}

case class PeaseFft(config: PeaseFftConfig) extends TransformModule[ComplexFix, ComplexFix] {

  import config._
  import algos.Matrices.{digitReversalPermutation, stridePermutation}
  import algos.Dft.diagC

  val dataType = HardType(SFix(0 exp, -(dataWidth - 1) exp))
  val coeffType = HardType(SFix(1 exp, -(coeffWidth - 2) exp))

  val innerMax = n + 1 / 2
  val innerType = HardType(SFix(innerMax exp, -(dataWidth - innerMax - 1) exp))

  override val dataIn = slave Flow Fragment(Vec(ComplexFix(dataType), N))
  override val dataOut = master Flow Fragment(Vec(ComplexFix(innerType), N))

  val dft = BaseDft.radixRDft(radix)
  val dftLatency = BaseDft.latency(radix)

  val R = digitReversalPermutation[Int](N, radix)
  val afterR = SpatialPermutation(dataIn.fragment, R)

  def iterativeBox(dataIn: Seq[ComplexFix], l: Int) = {
    val C = diagC(N, l, radix, inverse)
    val L = stridePermutation[Int](N, radix)
    val afterMult = dataIn.zip(C.diag.toArray).map { case (data, coeff) =>
      val product = data * CF(coeff, coeffType)
      product.truncated(innerType)
    }
    val afterDft = afterMult.grouped(radix).toSeq.flatMap(seq => dft(Vec(seq)).map(_.truncated(innerType)))
    val afterPerm = SpatialPermutation(afterDft, L)
    afterPerm
  }

  val dataPath = ArrayBuffer[Seq[ComplexFix]](afterR)

  // in our implementation, 1/N is not implemented in DFTs
  // ifft should be shift right n (for 1/N) and then shifted left n/2 (for normalization)
  // as a result, it should be shifted right (n+1)/2, same as fft
  def normalizedWidthOnStage(stage: Int) = {
    if (stage == 0) 0
    else (stage * stageWidth + 1) / 2
  }

  (0 until stageCount).reverse.map { i =>
    val next = iterativeBox(dataPath.last, i)
    val normalizeWidth = normalizedWidthOnStage(i + 1) - normalizedWidthOnStage(i)
    val normalized = next.map(_ >> normalizeWidth).map(_.truncated(innerType))
    //    println(s"normalized by $normalizeWidth")
    dataPath += normalized
  }

  dataOut.fragment := Vec(dataPath.last)
  autoValid()
  autoLast()
}
