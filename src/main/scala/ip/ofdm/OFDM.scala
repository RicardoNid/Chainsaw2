package org.datenlord
package ip.ofdm

import flowConverters._
import arithmetic.{LUT, LUTConfig}
import ip.convenc.{ConvEnc, ConvEncConfig}
import ip.fft.{PeaseFft, PeaseFftConfig}

import spinal.core._
import spinal.lib._

import scala.util.Random

// TODO: this should be a system, rather than a Transform
case class OFDMConfig(override val spaceFold: Int)
  extends BaseTransformConfig {

  override val size = (128, 64)

  override def latency = 1

  override def implH = OFDM(this)

  val convConfig = ConvEncConfig(Seq(Seq("171", "133")))
  val spConfig = StridePermutationFor2Config(8, 8 - log2Up(spaceFold), 4, 1)
  val qamConfig = LUTConfig(Random.RandomComplexSequences(1, 16).head, HardType(ComplexFix(SFix(0 exp, -15 exp))))
  val ifftConfig = PeaseFftConfig(64, 2, 16, 12, inverse = true, spaceFold, 1)

  // TODO: reference model
}

case class OFDM(config: OFDMConfig) extends TransformModule[Bool, ComplexFix] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bool(), 128 / spaceFold))
  override val dataOut = master Flow Fragment(Vec(ComplexFix(4 exp, -11 exp), 64 / spaceFold))

  val convCores = Seq.fill(128)(convConfig.implH)
  val intrlvCore = StridePermutationFor2(spConfig)
  val qamCores = Seq.fill(64)(LUT(qamConfig))
  val ifftCore = PeaseFft(ifftConfig)

  // sliding input
  val tail = RegNext(Vec(dataIn.fragment.takeRight(convConfig.constraintLength - 1)))
  // convenc
  val intoConv = (tail ++ dataIn.fragment).sliding(convConfig.inputWidth, convConfig.inputStep).toSeq
  convCores.zip(intoConv).foreach { case (conv, bools) =>
    conv.dataIn.fragment := bools
    conv.skipControl()
  }
  val afterConv: Seq[Bool] = convCores.flatMap(_.dataOut.fragment)
  // intrlv
  intrlvCore.dataIn.fragment := Vec(afterConv.map(B(_)))
  intrlvCore.skipControl()
  val afterIntrlv = intrlvCore.dataOut.fragment
  // qam
  val intoQam = afterIntrlv.grouped(4).toSeq.map(_.asBits().asUInt)
  println(intoQam.length)
  println(intoQam.map(_.getBitsWidth).mkString(" "))
  qamCores.zip(intoQam).foreach { case (qam, addr) =>
    qam.dataIn.fragment := Vec(addr)
    qam.skipControl()
  }
  // ifft
  val afterQam = qamCores.flatMap(_.dataOut.fragment)
  ifftCore.dataIn.fragment := Vec(afterQam)
  ifftCore.skipControl()
  dataOut.fragment := ifftCore.dataOut.fragment
  autoValid()
  autoLast()
}