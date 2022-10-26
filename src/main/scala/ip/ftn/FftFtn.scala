package org.datenlord
package ip.ftn

import breeze.linalg.DenseVector
import breeze.math._
import breeze.signal.fourierTr
import org.datenlord.dsp.CtFft
import org.datenlord.flowConverters.P2S
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object RvFftModel {

  def pre(dataIn: Seq[Any]): Seq[Complex] = {
    val both = dataIn.asInstanceOf[Seq[Double]] // (N2*2+20) * 2
    val withCp = both.grouped(widthWithCp).toSeq
    val paddedWithoutCp = withCp.map(seq => seq.drop(cpLenth).padTo(512, 0.0))
    paddedWithoutCp.head.zip(paddedWithoutCp.last).map { case (a, b) => Complex(a, b) } // 512
  }

  def post(dataIn: Seq[Any]): Seq[Complex] = {
    val complex = dataIn.asInstanceOf[Seq[Complex]]
    val symmetric = (complex.head +: complex.tail.reverse).map(_.conjugate)
    val out0 = complex.zip(symmetric).map { case (com, sym) => com + sym }
    val out1 = complex.zip(symmetric).map { case (com, sym) => (com - sym) / i }
    out0.slice(2, 2 + N1) ++ out1.slice(2, 2 + N1)
  }

  def doOnce(dataIn: Seq[Any]) = {
    val afterPre = pre(dataIn)
    val fft = fourierTr.dvComplex1DFFT(DenseVector(afterPre.toArray))
    val afterPost = post(fft.toArray)
    afterPost
  }

}

object RvFftPre extends ChainsawGenerator {
  override def name = "rvFftPre"

  override def impl(dataIn: Seq[Any]) = RvFftModel.pre(dataIn)

  override var inputTypes = Seq.fill(txRxWidth)(fftType.toSFixInfo)
  override var outputTypes = Seq.fill(64)(fftType)

  override var inputFormat = MatrixFormat(txRxWidth, 8)
  override var outputFormat = MatrixFormat(64, 8)

  val p2sBlockGen = P2S(128, 64, fftType.bitWidth, 4)

  override var latency = p2sBlockGen.latency + 5

  override def implH: ChainsawModule = new ChainsawModule(this) {

    def sfixZero = fftType.toSFixInfo.asSFix().getZero

    val part1 = sfixDataIn
    val part0 = part1.d(1)

    val counterForExtract = CounterFreeRun(4)
    when(delayedLast(1))(counterForExtract.clear())

    val beforeSlicing = part0 ++ part1 // to be sliced from
    val padWidth = 512 - (widthWithCp - 20)

    val padded = Vec(fftType.toSFixInfo.asSFix(), 128)
    switch(counterForExtract.value) {
      is(0)(padded := beforeSlicing.slice(20, 20 + 128))
      is(1)(padded := beforeSlicing.slice(16, 16 + 128))
      is(2)(padded := beforeSlicing.slice(12, 12 + 128))
      is(3)(padded := (beforeSlicing.slice(8, 8 + 124) ++ Seq.fill(padWidth)(sfixZero)))
    }

    val paddedDelayed = padded.d(4)
    val combined = paddedDelayed.zip(padded).map { case (r, i) => ComplexFix(r, i) }

    val p2s = p2sBlockGen.implH
    p2s.dataIn := combined.map(_.asBits)
    p2s.lastIn := delayedLast(5)

    dataOut := p2s.dataOut
  }
}

object RvfftPost extends ChainsawGenerator {
  override def name = "rvFftPost"

  override def impl(dataIn: Seq[Any]) = RvFftModel.post(dataIn)

  override var inputTypes = Seq.fill(64)(fftType)
  override var outputTypes = Seq.fill(64)(fftType)

  override var inputFormat = MatrixFormat(64, 8)
  override var outputFormat = fftFrame

  override var latency = 8

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val history = History(complexDataIn, 8)

    val reversed = cloneOf(complexDataIn)
    switch(localCounter.value) {
      is(7)(reversed := history(7).head +: history(0).tail.reverse)
      is(0)(reversed := history(1).head +: history(2).tail.reverse)
      is(1)(reversed := history(3).head +: history(4).tail.reverse)
      is(2)(reversed := history(5).head +: history(6).tail.reverse)
      default(reversed.assignDontCare())
    }

    val delayed = history(7)
    delayed.zipWithIndex.foreach { case (fix, i) => fix.setName(s"delayedInput_$i") }
    val conjugated = reversed.map(_.conj)
    val sum = delayed.zip(conjugated).map { case (a, b) => (a + b).d(1) }
    val diff = delayed.zip(conjugated).map { case (a, b) => (a.d(4) - b.d(4)).d(1).divideI }

    val takeDiff = afterTime(3)
    complexDataOut := Mux(takeDiff, Vec(diff), Vec(sum))
  }
}

object RvFftftn extends ChainsawGenerator {
  override def name = "RvFftFtn"

  val scales = Seq(2, 2, 1)
  val factors = Seq(8, 8, 8)
  val ifftCoreGen = CtFft(512, inverse = false, fftType, 16, factors, scales, 64)
  val combinedGen = RvFftPre -> ifftCoreGen -> RvfftPost

  override def impl(dataIn: Seq[Any]): Seq[Any] = {
    dataIn.asInstanceOf[Seq[Complex]].grouped(widthWithCp * 2).toSeq.flatMap(combinedGen.impl)
  }

  override var inputTypes = combinedGen.inputTypes
  override var outputTypes = combinedGen.outputTypes

  override var inputFormat = txRxFrameFormatWithPreamble
  override var outputFormat = paddedFrameFormat

  override var latency = combinedGen.latency

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val core = combinedGen.implH
    core.dataIn := dataIn
    core.lastIn := periodicTrigger(8)
    dataOut := core.dataOut
  }
}
