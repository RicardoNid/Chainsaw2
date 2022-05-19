package org.datenlord
package flowConverters

import algos.Matrices
import flowConverters.StridePermutationFor2.matrixJ
import flowConverters.Utils.DSD

import breeze.linalg._
import spinal.core._
import spinal.lib.{Counter, _}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** Stride permutation for which frame, stride and stream width are all 2's exponent, based on Registers
 *
 * @param n        frame length
 * @param q        stream width
 * @param s        stride length
 * @param bitWidth bit width of elements in input & output vector
 * @see ''Stride Permutation Networks for Array Processors'', Tuomas J¨arvinen Perttu Salmela Harri Sorokin Jarmo Takala
 */
case class StridePermutationFor2Config(n: Int, q: Int, s: Int, bitWidth: Int)
  extends TransformBase {

  val N = 1 << n
  val Q = 1 << q
  val S = 1 << s
  // TODO: implement the inverse network when r != s
  val r = min(s, n - s)
  val R = 1 << r

  val networkType = // there cases, each has a corresponding network structure
    if (q == n) -1
    else if (q > n - r) 0
    else if (q <= n - r && q >= r) 1
    else 2 // q < r


  override val size = (N, N)

  override def latency =
    networkType match {
      case -1 => 0
      case 0 => MTNConfig(n - q, bitWidth).latency
      case 1 => MTNConfig(r, bitWidth).latency + SPNConfig(n - q, r, bitWidth).latency
      case 2 => SPNConfig(r, r - q, bitWidth).latency + MTNConfig(q, bitWidth).latency + SPNConfig(n - q, r, bitWidth).latency
    }

  override val spaceFold = N / Q

  override def impl(dataIn: Seq[Any]) = dataIn.grouped(1 << s).toSeq.transpose.flatten

  override def implH = StridePermutationFor2(this)
}

object StridePermutationFor2Config {

}

case class StridePermutationFor2(config: StridePermutationFor2Config) extends TransformModule[Bits, Bits] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidth bits), Q))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidth bits), Q))

  logger.info(s"generating radix-2 stride permutation P_{$N, $S} based on registers, case $networkType")

  def passP(perm: DenseMatrix[Int], dataIn: TempFlow) = {
    val retData = Vec(SpatialPermutation(dataIn.fragment, perm))
    TempFlow(retData, dataIn.valid, dataIn.last)
  }

  def passMTN(x: Int, dataIn: TempFlow) = {
    val config = MTNConfig(x, bitWidth)
    val MTNs = Seq.fill(1 << (q - x))(MTN(config))
    val segments = dataIn.fragment.grouped(1 << x).toSeq
    MTNs.zip(segments).foreach { case (mtn, data) =>
      mtn.dataIn.fragment := Vec(data)
      mtn.dataIn.valid := dataIn.valid
      mtn.dataIn.last := dataIn.last
    }
    val retData = Vec(MTNs.flatMap(_.dataOut.fragment))
    TempFlow(retData, MTNs.head.dataOut.valid, MTNs.head.dataOut.last)
  }

  def passSPN(n: Int, s: Int, dataIn: TempFlow) = {
    val config = SPNConfig(n, s, bitWidth)
    val SPNs = Seq.fill(1 << q)(SPN(config))
    SPNs.zip(dataIn.fragment).foreach { case (spn, data) =>
      spn.dataIn.fragment := Vec(data)
      spn.dataIn.valid := dataIn.valid
      spn.dataIn.last := dataIn.last
    }
    val retData = Vec(SPNs.flatMap(_.dataOut.fragment))
    TempFlow(retData, SPNs.head.dataOut.valid, SPNs.head.dataOut.last)
  }

  val FlowIn = TempFlow(dataIn.fragment, dataIn.valid, dataIn.last)

  networkType match {
    case -1 =>
      val P = Matrices.stridePermutation[Int](N, S)
      dataOut.fragment := Vec(SpatialPermutation(dataIn.fragment, P))
    case 0 =>
      val P0 = {
        val core = Matrices.stridePermutation[Int](R, 1 << (q + r - n))
        Matrices.kronecker(core, 1 << (q - r))
      }
      val P1: DenseMatrix[Int] = Matrices.stridePermutation[Int](Q, R)

      val afterP0 = passP(P0, FlowIn)
      val afterMTNs = passMTN(n - q, afterP0)
      val afterP1 = passP(P1, afterMTNs)
      dataOut.fragment := afterP1.fragment

    case 1 =>
      val P = Matrices.stridePermutation[Int](Q, R)

      val afterMTNs = passMTN(r, FlowIn)
      val afterP = passP(P, afterMTNs)
      val afterSPNs = passSPN(n - q, r, afterP)
      dataOut.fragment := afterSPNs.fragment

    case 2 =>
      val afterSPNs0 = passSPN(r, r - q, FlowIn)
      val afterMTNs = passMTN(q, afterSPNs0)
      val afterSPNs1 = passSPN(n - q, r, afterMTNs)
      dataOut.fragment := afterSPNs1.fragment
  }

  autoValid()
  autoLast()
}

object StridePermutationFor2 {

  def matrixJ(K: Int) = {
    if (K == 2) DenseMatrix.eye[Int](2)
    else {
      val P0 = Matrices.stridePermutation[Int](K / 2, K / 4)
      val P1 = Matrices.stridePermutation[Int](K, 2)
      Matrices.kronecker(P0, 2) * P1
    }
  }

  def main(args: Array[String]): Unit = {
    val J4 = matrixJ(4)
    val J8I2 = Matrices.kronecker(matrixJ(8), DenseMatrix.eye[Int](2))
    val J8I1 = Matrices.kronecker(matrixJ(8), DenseMatrix.eye[Int](1))
    val J16I1 = Matrices.kronecker(matrixJ(16), DenseMatrix.eye[Int](1))
    println(SpatialPermutation((0 until 4), J4).mkString(" "))
    println(SpatialPermutation((0 until 16), J8I2).mkString(" "))
    println(SpatialPermutation((0 until 8), J8I1).mkString(" "))
    println(SpatialPermutation((0 until 16), J16I1).mkString(" "))
  }
}

case class MTNConfig(q: Int, bitWidth: Int)
  extends TransformBase {

  val Q = 1 << q
  val N = Q * Q


  override val size = (N, N)

  override def latency = Q

  override val spaceFold = Q

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    data.grouped(Q).toSeq.transpose.flatten
  }

  override def implH = MTN(this)
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

case class SPNConfig(n: Int, s: Int, bitWidth: Int)
  extends TransformBase {

  val N = 1 << n
  val S = 1 << s

  val networkType = // there cases, each has a corresponding network structure
    if (n % 2 == 0 && s == n / 2) 0 else 1


  override val size = (N, N)

  override def latency = networkType match {
    case 0 => ((1 << s) - 1) * ((1 << s) - 1)
    case 1 => ((1 << s) - 1) * ((1 << (n - s)) - 1)
  }

  override val spaceFold = N

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    data.grouped(S).toSeq.transpose.flatten
  }

  override def implH = SPN(this)
}

case class SPN(config: SPNConfig) extends TransformModule[Bits, Bits] {

  import config._

  //  logger.info(s"generating SPN $N $S, case $networkType")

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidth bits), 1))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidth bits), 1))

  networkType match {
    case 0 =>
      val counters = (0 until s).map(i => CounterFreeRun(1 << (s + 1 + i)))
      val controls = counters.map { counter =>
        val det = counter.value.takeHigh(s + 1)
        det.msb && ~det.lsb
      }
      counters.foreach(counter => when(dataIn.last)(counter.clear()))

      val baseLatency = (1 << s) - 1

      val dataPath = ArrayBuffer[Bits](dataIn.fragment.head)
      (0 until s).foreach { i =>
        val seuSize = (1 << i) * ((1 << s) - 1)
        val prevLatency = ((1 << i) - 1) * baseLatency
        val next = Utils.SEU(seuSize, dataPath.last, controls(i).d(prevLatency))
        dataPath += next
      }

      dataOut.fragment := Vec(dataPath.last)

    case 1 =>
      val exps = Seq.tabulate(n - s, s)((i, j) => i - j + s - 1).flatten
      val latencies = exps.map(1 << _).inits.toSeq.map(_.sum).tail.reverse
      val counters = exps.map(i => CounterFreeRun(1 << (i + 2)))
      counters.foreach(counter => when(dataIn.last)(counter.clear()))
      val controls = counters.map { counter =>
        val det = counter.value.takeHigh(2)
        det.msb && ~det.lsb
      }
      val dataPath = ArrayBuffer[Bits](dataIn.fragment.head)
      exps.zip(latencies).zipWithIndex.foreach { case ((exp, latency), i) =>
        val seuSize = 1 << exp
        val next = Utils.SEU(seuSize, dataPath.last, controls(i).d(latency))
        dataPath += next
      }
      dataOut.fragment := Vec(dataPath.last)
  }

  autoValid()
  autoLast()
}
