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

  val stageCount = logR(N, radix)

  override def latency = stageCount * (6 + BaseDft.latency(radix))

  override def inputFlow = FullyPipelinedFlow(N)

  override def outputFlow = FullyPipelinedFlow(N)

  override def complexTransform(dataIn: Seq[Complex]) = {
    val dftMatrix = algos.Dft.dftMatrix(N, false)
    val ret = dftMatrix * new DenseVector(dataIn.toArray) / Complex(sqrt(N.toDouble), 0) // for normalization
    ret.toArray.toSeq
  }
}

case class PeaseFft(config: PeaseFftConfig) extends TransformModule[ComplexFix, ComplexFix] {

  import config._
  import algos.Matrices.{digitReversalPermutation, stridePermutation}
  import algos.Dft.diagC

  val dataType = HardType(SFix(3 exp, -(dataWidth - 4) exp))
  val coeffType = HardType(SFix(3 exp, -(coeffWidth - 4) exp))

  override val dataIn = slave Flow Fragment(Vec(ComplexFix(dataType), N))
  override val dataOut = master Flow Fragment(Vec(ComplexFix(dataType), N))

  val dft = BaseDft.radixRDft(radix)
  val dftLatency = BaseDft.latency(radix)

  val R = digitReversalPermutation[Int](N, radix)
  val afterR = SpatialPermutation(dataIn.fragment, R)

  def iterativeBox(dataIn: Seq[ComplexFix], l: Int) = {
    val C = diagC(N, l, radix, inverse)
    val L = stridePermutation[Int](N, radix)
    val afterMult = dataIn.zip(C.diag.toArray).map { case (data, coeff) =>
      val product = data * CF(coeff, coeffType)
      product.truncated(dataType)
    }
    val afterDft = afterMult.grouped(radix).toSeq.flatMap(seq => dft(Vec(seq)).map(_.truncated(dataType)))
    val afterPerm = SpatialPermutation(afterDft, L)
    afterPerm
  }

  val dataPath = ArrayBuffer[Seq[ComplexFix]](afterR)
  (0 until stageCount).reverse.map { i =>
    dataPath += iterativeBox(dataPath.last, i)
  }

  dataOut.fragment := Vec(dataPath.last)
  autoValid()
  autoLast()
}
