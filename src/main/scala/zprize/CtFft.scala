package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import CommonAlgos.WNnk

import scala.language.postfixOps

/** fully pipelined Cooley-Tukey FFT
 *
 * @param N
 * @param inverse
 * @param dataType
 * @param coeffWidth
 * @param factors decomposition factors
 * @param scales  scaling down bits number at each stages
 */
case class CtFftCore(N: Int, inverse: Boolean, // determining the transformation
                     dataType: NumericTypeInfo, coeffWidth: Int, // determining the precision
                     factors: Seq[Int], scales: Seq[Int]) // determining the decomposition and scaling on each stage
  extends ChainsawGenerator {

  // TODO: implement scaling
  require(factors.product == N)
  val radixList = Seq(2, 4, 8)
  require(factors.forall(radixList.contains(_)))
  require(factors.length == scales.length)

  val prefix = if (inverse) "ifft" else "fft"

  override def name = s"${prefix}_n${N}_factors_${factors.mkString("_")}_scales_${scales.mkString("_")}"

  override val impl = (dataIn: Seq[Any]) => {
    val data = dataIn.asInstanceOf[Seq[Complex]].toArray
    if (inverse) iFourierTr.dvComplexIFFT(DenseVector(data)).toArray.toSeq.map(_ * N / (1 << scales.sum))
    else fourierTr.dvComplex1DFFT(DenseVector(data)).toArray.toSeq.map(_ / (1 << scales.sum))
  }

  override var inputTypes = Seq.fill(N)(dataType)
  override var outputTypes = Seq.fill(N)(dataType)

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  // preparing generators for DFT
  val dftGens = factors.distinct.map(i => i -> Dft(i, inverse, dataType, coeffWidth)).toMap
  val exampleTwiddle = Twiddle(N, 0, dataType, coeffWidth)

  override var latency = factors.map(dftGens).map(_.latency).sum + // dft latency
    (factors.length - 1) * exampleTwiddle.latency // twiddle latency

  override def implH: ChainsawModule = new ChainsawModule(this) {

    def matIntrlv[T](input: Seq[T], row: Int, col: Int): Seq[T] = Seq.tabulate(col, row)((i, j) => j * col + i).flatten.map(input(_))

    def transpose[T](input: Seq[Seq[T]]): Seq[Seq[T]] = {
      val row = input.size
      val col = input.head.size
      Seq.tabulate(col, row)((i, j) => input(j)(i)) // transpose
    }

    // real shift(rather than shifting the point)
    // TODO: for perfect scaling, carries should be allowed in DFT(DFT output type is not the same as DFT input type)
    def getScaled(data: Bits, scaleDown: Int): Bits = {
      val complex = dataType.asComplexFix()
      complex.assignFromBits(data)
      (complex >> scaleDown).truncate(dataType.asSFix).asBits
    }

    def doTwiddle(input: Seq[Seq[Bits]]) = {
      def indices(N1: Int, N2: Int) = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1)

      val N1 = input.head.size
      val N2 = input.size
      input.zip(indices(N1, N2))
        .map { case (ts, ints) => ts.zip(ints)
          .map { case (t, i) =>
            val index = if (!inverse) i else -i
            val N = N1 * N2
            Twiddle(N, index, dataType, coeffWidth).asFunc(Seq(t)).head
          }
        }
    }

    // recursively build the module
    def build(input: Seq[Bits], factors: Seq[Int], scales: Seq[Int]): Seq[Bits] = {
      val currentFactor = factors.head
      val currentScale = scales.head
      if (factors.size == 1) {
        val ret = dftGens(currentFactor).asFunc(input)
        ret.map(getScaled(_, currentScale))
      }
      else {
        val N1 = factors.head
        val N2 = input.size / N1
        val input2D = matIntrlv(input, N1, N2).grouped(N1).toSeq // permutation 0
        val afterBlock = input2D.map(dftGens(N1).asFunc)
        val afterScale = afterBlock.map(_.map(getScaled(_, currentScale)))
        val afterParallel = doTwiddle(afterScale)
        val input2DForRecursion = transpose(afterParallel) // permutation 1(transpose)
        // recursively call other stages
        val afterRecursion = input2DForRecursion.map(build(_, factors.tail, scales.tail))
        val ret = matIntrlv(afterRecursion.flatten, N1, N2) // permutation 2
        ret
      }
    }

    dataOut := build(dataIn, factors, scales)
  }
}

case class CtFft(N: Int, inverse: Boolean, // determining the transformation
                 dataType: NumericTypeInfo, coeffWidth: Int, // determining the precision
                 factors: Seq[Int], scales: Seq[Int], // determining the decomposition and scaling on each stage,
                 streamWidth: Int)

  extends ChainsawGenerator {

  require(factors.product == N)
  require(factors.scan(1)(_ * _).contains(streamWidth))
  require(streamWidth >= sqrt(N))
  val splitPoint = factors.scan(1)(_ * _).indexWhere(_ == streamWidth) // split point of factors & scales

  val prefix = if (inverse) "ifft" else "fft"

  override def name = s"${prefix}_n${N}_sw${streamWidth}_factors_${factors.mkString("_")}"

  override val impl = (dataIn: Seq[Any]) => {
    val data = dataIn.asInstanceOf[Seq[Complex]].toArray
    if (inverse) iFourierTr.dvComplexIFFT(DenseVector(data)).toArray.toSeq.map(_ * N / (1 << scales.sum))
    else fourierTr.dvComplex1DFFT(DenseVector(data)).toArray.toSeq.map(_ / (1 << scales.sum))
  }

  override var inputTypes = Seq.fill(streamWidth)(dataType)
  override var outputTypes = Seq.fill(streamWidth)(dataType)

  val format = MatrixFormat(streamWidth, N / streamWidth)
  override var inputFormat = format
  override var outputFormat = format

  // preparation for hardware generation
  val N0 = streamWidth // partA
  val N1 = N / streamWidth // part B
  val (factors0, factors1) = factors.splitAt(splitPoint)
  val (scales0, scales1) = scales.splitAt(splitPoint)
  // preparing generators for FFT cores
  val coreGen0 = CtFftCore(N0, inverse, dataType, coeffWidth, factors0, scales0)
  val coreGen1 = CtFftCore(N1, inverse, dataType, coeffWidth, factors1, scales1)
  val interGen0 = MatIntrlv(N0, N1, dataType.bitWidth, streamWidth) // inter 2 is the same as inter1
  val interGen1 = MatIntrlv(N1, N0, dataType.bitWidth, streamWidth)
  override var latency = coreGen0.latency + coreGen1.latency + // FFTs
    interGen0.latency * 2 + interGen1.latency + // matIntrlvs
    6 // complex multiplication

  def getIndicesBetween(N1: Int, N2: Int): Seq[Seq[Int]] = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1)

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val coeffType = HardType(SFix(1 exp, -(coeffWidth - 2) exp))

    // instantiations
    val inter0 = interGen0.implDut
    val inter1 = interGen1.implDut
    val inter2 = interGen0.implDut
    val core0 = coreGen0.implDut
    val core1s = Seq.fill(N0 / N1)(coreGen1.implDut)

    // get factors from ROMs
    val twiddleFactors: Seq[Seq[Complex]] = getIndicesBetween(N1, N0)
      .map(_.map(i => if (inverse) WNnk(N, -i) else WNnk(N, i)))
    val twiddleFactorROMs = twiddleFactors.map(vec => Mem(Vec(vec.map(CF(_, coeffType)))))
    val factorCounter = CounterFreeRun(N1)
    when(core0.dataOut.last)(factorCounter.clear())
    val currentFactors = twiddleFactorROMs.map(_.readSync(factorCounter.valueNext))

    // connections
    inter0.dataIn << bundleIn
    core0.dataIn << inter0.dataOut

    // core0 -> multiply twiddle factors -> inter1
    val multiplied = core0.dataOut.fragment
      .zip(currentFactors)
      .map { case (data, coeff) =>
        val complex = dataType.asComplexFix()
        complex.assignFromBits(data)
        (complex * coeff).truncate(dataType.asSFix).asBits
      }
    inter1.dataIn.fragment := multiplied
    inter1.dataIn.last := core0.dataOut.last.validAfter(6)
    inter1.dataIn.valid := core0.dataOut.valid.validAfter(6)

    // inter1 -> core1s
    core1s.zip(inter1.dataOut.fragment.grouped(N1).toSeq)
      .foreach { case (core, bits) => core.dataIn.fragment := bits }
    core1s.foreach(_.dataIn.last := inter1.dataOut.last)
    core1s.foreach(_.dataIn.valid := inter1.dataOut.valid)
    // core2s -> inter2
    inter2.dataIn.fragment := Vec(core1s.map(_.dataOut.payload.toSeq).reduce(_ ++ _))
    inter2.dataIn.valid := core1s.head.dataOut.valid
    inter2.dataIn.last := core1s.head.dataOut.last

    dataOut := inter2.dataOut.fragment
  }
}