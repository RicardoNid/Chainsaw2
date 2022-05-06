package org.datenlord
package dataFlow

import algos.Matrices
import dataFlow.StridePermutation2.matrixJ

import breeze.linalg._
import spinal.core._
import spinal.lib.{Counter, _}
import Utils.{DSD, SEU, switch22}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class StridePermutation2Config(n: Int, q: Int, s: Int, bitWidth: Int) extends TransformConfig {

  val N = 1 << n
  val Q = 1 << q
  val S = 1 << s
  val r = min(s, n - s)
  val R = 1 << r

  override def latency = 1 << (n - q)

  override def inputFlow = CyclicFlow(Q, N / Q)

  override def outputFlow = CyclicFlow(Q, N / Q)

  override def transform(dataIn: Seq[BigInt]) = dataIn.grouped(1 << s).toSeq.transpose.flatten
}

case class StridePermutation2(config: StridePermutation2Config) extends TransformModule[Bits, Bits] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidth bits), Q))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidth bits), Q))

  require(q > n - r) // case1
  println(s"I_${1 << (q - r)} P_{$R, ${1 << (q + r - n)}}")
  println(s"MTN_${1 << (n - q)}")
  println(s"P_{$Q, $R}")

  val P0 = Matrices.stridePermutation[Int](R, 1 << (q + r - n))
  val P1 = Matrices.stridePermutation[Int](Q, R)

  val mtnConfig = MTNConfig(n - q, bitWidth)
  val MTNs = Seq.fill(1 << (2 * q - n))(MTN(mtnConfig))

  val afterPart1: Seq[Seq[Bits]] = SpatialPermutation(dataIn.fragment, Matrices.kronecker(P0, 1 << (q - r))).grouped(1 << (n - q)).toSeq

  MTNs.zip(afterPart1).foreach { case (mtn, data) =>
    mtn.dataIn.fragment := Vec(data)
    mtn.dataIn.valid := dataIn.valid
    mtn.dataIn.last := dataIn.last
  }

  val afterMTNs: Vec[Bits] = Vec(MTNs.map(_.dataOut.fragment).flatten)
  val afterPart2 = SpatialPermutation(afterMTNs, P1)

  dataOut.fragment := Vec(afterPart2)
  autoValid()
  autoLast()
}

object StridePermutation2 {

  def matrixJ(K: Int) = {
    if (K == 2) DenseMatrix.eye[Int](2)
    else {
      val P0 = Matrices.stridePermutation[Int](K / 2, K / 4)
      val P1 = Matrices.stridePermutation[Int](K, 2)
      Matrices.kronecker(P0, 2) * P1
    }
  }

  def main(args: Array[String]): Unit = {
    println(matrixJ(2))
    println(matrixJ(4))
  }
}

case class MTNConfig(q: Int, bitWidth: Int) extends TransformConfig {

  val Q = 1 << q
  val N = Q * Q

  override def latency = Q

  override def inputFlow = CyclicFlow(Q, Q)

  override def outputFlow = CyclicFlow(Q, Q)

  override def transform(dataIn: Seq[BigInt]) = dataIn.grouped(Q).toSeq.transpose.flatten
}

case class MTN(config: MTNConfig) extends TransformModule[Bits, Bits] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidth bits), Q))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidth bits), Q))

  def makeMTN(X: Int, dataIn: Vec[Bits], controlIn: Seq[Bool]) = {
    require(dataIn.length == 1 << X && controlIn.length == X)
    val dataPath = ArrayBuffer[Vec[Bits]](dataIn)
    (0 until X).foreach { i =>
      val current = dataPath.last
      val Seq(odds, evens) = current.grouped(2).toSeq.transpose
      val afterDSD = Vec(odds.zip(evens).map { case (odd, even) => DSD(1 << i, odd, even, controlIn(i)) }.flatMap { case (a, b) => Seq(a, b) })
      val matrixHW = if (i == X - 1) Matrices.stridePermutation[Int](1 << X, 2) else Matrices.kronecker(matrixJ(1 << (i + 2)), 1 << (X - 2 - i))
      val next = Vec(SpatialPermutation(afterDSD, matrixHW))
      afterDSD.setName(s"AfterDsd$i")
      dataPath += next
      next.setName(s"AfterPerm$i")
    }
    dataPath.last
  }

  val counters = (1 to q).map(i => Counter(2))
  when(dataIn.last)(counters.foreach(_.clear()))
  counters.head.increment()
  counters.init.zip(counters.tail).foreach { case (prev, next) => when(prev.willOverflow)(next.increment()) }
  val controls = counters.zipWithIndex.map { case (counter, i) => counter.value.msb.d((1 << i) - 1) }

  val ret = makeMTN(q, dataIn.fragment, controls)
  dataOut.fragment := ret.d(1) // TODO: must delay, or, verilator failed on this part, why?

  autoLast()
  autoValid()
}

case class SPNConfig(n: Int, s: Int, bitWidth: Int) extends TransformConfig {

  val N = 1 << n
  val S = 1 << s

  override def latency = 1

  override def inputFlow = CyclicFlow(1, N)

  override def outputFlow = CyclicFlow(1, N)

  override def transform(dataIn: Seq[BigInt]) = dataIn.grouped(S).toSeq.transpose.flatten
}

case class SPN(config: SPNConfig) extends TransformModule[Bits, Bits] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidth bits), 1))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidth bits), 1))

  val counter = CounterFreeRun(2)
  when(dataIn.last)(counter.clear())

  val dataPath = ArrayBuffer[Bits](dataIn.fragment.head)
  (0 until s).foreach { i =>
    val seuSize = (1 << i) * ((1 << s) - 1)
    println(seuSize)
    Utils.SEU((1 << i) * ((1 << s) - 1), dataPath.last, counter.value.msb)
  }

  dataOut.fragment := Vec(dataPath.last)
  autoValid()
  autoLast()
}
