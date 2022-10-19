package org.datenlord
package zprize

import breeze.linalg.DenseVector
import breeze.math._
import breeze.signal.iFourierTr
import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

object HsIfftPre extends ChainsawGenerator {
  override def name = "hsIfftPre"

  override val impl = (dataIn: Seq[Any]) => {
    // FIXME: change the golden
    val both = dataIn.asInstanceOf[Seq[Complex]]
    val (as, bs) = both.splitAt(256)
    val symA =  (as :+ Complex(0, 0)) ++ as.tail.map(_.conjugate).reverse
    val symB =  (bs :+ Complex(0, 0)) ++ bs.tail.map(_.conjugate).reverse
    symA.zip(symB).map { case (a, b) => a + b * i }
  }

  override var inputTypes = Seq.fill(256)(fftType)
  override var outputTypes = Seq.fill(64)(fftType)

  override var inputFormat = MatrixFormat(256, 2).interpolate(4)
  override var outputFormat = MatrixFormat(64, 8)

  val p2sGen = P2S(512, 64, fftType.bitWidth)

  override var latency = 4 + 1 + p2sGen.latency

  override def implH: ChainsawModule = new ChainsawModule(this) {
    // preprocessing
    val a = RegNextWhen(complexDataIn, atTime(0))
    val b = complexDataIn
    val symmetricA =  (a :+ zero) ++ a.map(_.conj).reverse
    val symmetricB =  (b :+ zero) ++ b.map(_.conj).reverse
    val merged = symmetricA.zip(symmetricB).map { case (prev, next) => (prev + next.multiplyI).d(1) } // latency = 4 + 1 = 5

    def zero: ComplexFix = CF(Complex(0, 0), fftType.asSFix)

//    val symmetric = (merged :+ zero) ++ merged.map(_.conj).reverse
    // p2s
    val p2s = p2sGen.implH
    p2s.dataIn := merged.map(_.asBits)
    p2s.lastIn := atTime(5 - 1)
    p2s.validIn.assignDontCare()

    dataOut := p2s.dataOut
  }
}


object HsIfftPost extends ChainsawGenerator {
  override def name = "hsIfftPost"

  override val impl = (dataIn: Seq[Any]) => {
    val complex = dataIn.asInstanceOf[Seq[Complex]]
    val (real, imag) = (complex.map(_.real), complex.map(_.imag))
    real ++ imag
  }

  override var inputTypes = Seq.fill(512)(fftType)
  override var outputTypes = Seq.fill(512)(fftType.toSFixInfo)

  override var inputFormat = MatrixFormat(512, 1).interpolate(8)
  override var outputFormat = MatrixFormat(512, 2).interpolate(4)

  override var latency = 1

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val whole = RegNextWhen(complexDataIn, atTime(0))
    val real = Vec(whole.map(_.real))
    val imag = Vec(whole.map(_.imag))
    sfixDataOut := Mux(beforeTime(4 + 1), real, imag)
  }
}

object HsIfftPostWrapped extends ChainsawGenerator {
  override def name = "hsIfftPostWrap"

  override val impl = (dataIn: Seq[Any]) => {
    val complex = dataIn.asInstanceOf[Seq[Complex]]
    val (real, imag) = (complex.map(_.real), complex.map(_.imag))
    real ++ imag
  }

  override var inputTypes = Seq.fill(64)(fftType)
  override var outputTypes = Seq.fill(512)(fftType.toSFixInfo)

  override var inputFormat = MatrixFormat(64, 8)
  override var outputFormat = MatrixFormat(512, 2).interpolate(4)

  val s2pGen = S2P(64, 512, fftType.bitWidth)

  override var latency = s2pGen.latency + 1

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val core = HsIfftPost.implDut
    val s2p = s2pGen.implDut

    bundleIn >> s2p.dataIn
    s2p >> core
    dataOut := core.dataOut.fragment
  }
}

object IfftFtn extends ChainsawGenerator {

  override def name = "ifftFtn"

  val factors = Seq(8, 8, 8)
  val scales = Seq(2, 2, 1)

  override val impl = (dataIn: Seq[Any]) => {
    val all = dataIn.asInstanceOf[Seq[Complex]]

    def doOnce(both: Seq[Complex]) = {
      val (as, bs) = both.splitAt(N1)

      def hs(data: Seq[Complex]) = (data :+ Complex(0, 0)) ++ data.tail.map(_.conjugate).reverse

      def pad(data: Seq[Complex]) = {
        val zero = Complex(0, 0)
        Seq.fill(2)(zero) ++ data ++ Seq.fill(256 - N1 - 2)(zero)
      }

      val (padA, padB) = (pad(as), pad(bs))
      val (hsA, hsB) = (hs(padA), hs(padB))
      val merged = hsA.zip(hsB).map { case (a, b) => a + b * i }
      //      val hs = merged
      //      val hs = (merged :+ Complex(0, 0)) ++ merged.tail.map(_.conjugate).reverse

      val ret = iFourierTr.dvComplexIFFT(DenseVector(merged.toArray)).toArray.toSeq
        .map(_ * 512 / (1 << scales.sum))
      val (retA, retB) = (ret.map(_.real), ret.map(_.imag))

      def extractAndCp(whole: Seq[Double]) = {
        val extracted = whole.slice(0, N2 * 2)
        extracted.takeRight(20) ++ extracted
      }

      extractAndCp(retA) ++ extractAndCp(retB)
    }

    all.grouped(N1 * 2).toSeq.flatMap(doOnce)
  }

  override var inputTypes = Seq.fill(N1)(fftType)
  override var outputTypes = Seq.fill(N2 / 2 + 5)(fftType.toSFixInfo)

  override var inputFormat = symbolFrameFormat
  override var outputFormat = realFrameFormat

  val preGen = HsIfftPre
  val ifftCoreGen = CtFft(512, inverse = true, fftType, 16, factors, scales, 64)
  val s2pGen = S2P(64, 512, fftType.bitWidth)
  val postGen = HsIfftPost
  val p2sGen = P2S(N2 * 2 + 20, N2 / 2 + 5, fftType.toSFixInfo.bitWidth)
  val gens = Seq(preGen, ifftCoreGen, s2pGen, postGen, p2sGen)

  override var latency = gens.map(_.latency).sum

  override def implH: ChainsawModule = new ChainsawModule(this) {

    // generators
    val pre = preGen.implDut
    val ifftCore = ifftCoreGen.implDut
    val s2p = s2pGen.implDut
    val post = postGen.implDut
    val p2s = p2sGen.implDut

    // extra processing
    // pad
    def pad(data: Seq[Bits]): Seq[Bits] = {
      def zero: Bits = B(0, fftType.bitWidth bits)
      Seq.fill(2)(zero) ++ data ++ Seq.fill(256 - N1 - 2)(zero)
    }

    // +CP & extract
    def extractAndCp(data: Seq[Bits]): Seq[Bits] = {
      val extracted = data.slice(0, N2 * 2)
      extracted.takeRight(20) ++ extracted
    }

    // connections
    bundleIn.replaceBy(pad).withLast(periodicTrigger(8)) >> pre.dataIn
    pre >> ifftCore >> s2p >> post
    post.dataOut.replaceBy(extractAndCp) >> p2s.dataIn
    dataOut := p2s.dataOut.fragment
  }
}
