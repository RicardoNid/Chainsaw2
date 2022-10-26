package org.datenlord
package ip

import org.datenlord.ChainsawMetric._
import org.datenlord.matlab._

import scala.reflect.ClassTag

package object ftn {

  /** --------
   * algo parameters
   * -------- */
  val N1 = 254
  val N2 = 254

  val foldingFactor = 2
  val parallel = N1 / foldingFactor
  val fecLength = 32 * foldingFactor

  val frameSize = N1 * 4 * 16
  val fullSize = 256 * 4 * 16
  val preambleSize = frameSize / 8

  val cpLenth = 20
  val txFrameSize = (N2 * 2 + cpLenth) * 16

  val intrlvRow = N1 / 2
  val intrlvCol = 4 * 4 * 16

  val fftSize = 512
  val widthWithCp = N2 * 2 + 20
  val txRxWidth = widthWithCp / 4

  val bitAlloc = Seq.fill(N1)(4)
  val powAlloc = Seq.fill(N1)(1.0)

  /** --------
   * golden from simulink
   * -------- */

  def readFtnGolden[T: ClassTag](name: String): Array[T] = {
    matlabEngine.eval(s"value = load('./src/main/resources/simulink/FTN/data/$name', 'ans').ans.Data;")
    matlabEngine.eval(s"if size(value, 1) == 101\n\tvalue=value';\nend")
    val matrix = matlabEngine.getVariable("value").asInstanceOf[Array[Array[T]]]
    matrix.transpose.flatten
  }

  lazy val raw = readFtnGolden[Double]("rawData").map(d => BigInt(d.toInt)).toSeq
  lazy val coded = readFtnGolden[Double]("coded").map(d => BigInt(d.toInt)).toSeq
  lazy val interleaved = readFtnGolden[Double]("interleaved").map(d => BigInt(d.toInt)).toSeq
  lazy val symbols = readFtnGolden[MComplex]("mapped").toSeq.map(_.toComplex)
  lazy val ifftOut = readFtnGolden[Double]("dataTx").toSeq.map(_ * 16) // as simulink ifft is not scaled

  lazy val fftIn = readFtnGolden[Double]("rxAll").toSeq.map(_ * 16)
  lazy val fftOut = readFtnGolden[MComplex]("afterFft").toSeq.map(_.toComplex.conjugate) // FIXME: complex value read from matlab is wrong(conjugated), why?
  lazy val equalizationOut = readFtnGolden[MComplex]("afterEqualization").toSeq.map(_.toComplex.conjugate) // FIXME: complex value read from matlab is wrong(conjugated), why?
  lazy val qamdemodOut = readFtnGolden[Double]("afterQamdemod").toSeq.map(d => BigInt(d.toInt))
  lazy val deinterleaveOut: Seq[BigInt] = readFtnGolden[Double]("afterDeInterleave").map(d => BigInt(d.toInt)).toSeq
  lazy val viterbiOut: Seq[BigInt] = readFtnGolden[Double]("afterViterbi").map(d => BigInt(d.toInt)).toSeq

  /** --------
   * hardware data types
   * -------- */
  val symbolType = ComplexFixInfo(6, 11)
  val fftType = ComplexFixInfo(6, 11)

  /** --------
   * frame formats
   * -------- */

  val fullChannel = 0 until N1
  val paddedChannel = Seq.fill(2)(-1) ++ (0 until N1) ++ Seq.fill(256 - 2 - N1)(-1)
  val preambleCycle = 8
  val framePeriod = 64

  val rawFrameFormat = MatrixFormat(N1 / 2, framePeriod).pad(preambleCycle)
  val codedFrameFormat = MatrixFormat(N1, framePeriod).pad(preambleCycle)
  val symbolFrameFormat = MatrixFormat(N1, framePeriod / 4).interpolate(4).pad(preambleCycle)
  val txRxFrameFormat = MatrixFormat(txRxWidth, framePeriod).pad(preambleCycle)

  val fftFrame = FrameFormat(paddedChannel, 64).repeat(2)

  val paddedFrameFormat = fftFrame.repeat(9)
  val txRxFrameFormatWithPreamble = MatrixFormat(txRxWidth, framePeriod + preambleCycle)

  /** --------
   * metrics
   * -------- */

  def doubleMetricFtn(epsilon: Double, ber: Double) = ChainsawMetric(
    doubleBound(epsilon),berBound(ber, doubleBound(epsilon))
  )

  def complexMetricFtn(epsilon: Double, ber: Double) = ChainsawMetric(
   complexBound(epsilon), berBound(ber, complexBound(epsilon))
  )

  def bitMetricFtn(ber:Double) = ChainsawMetric(
    defaultBound, berBound(ber, defaultBound)
  )
}
