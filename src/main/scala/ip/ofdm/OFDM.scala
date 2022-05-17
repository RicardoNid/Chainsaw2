package org.datenlord
package ip.ofdm

import flowConverters._
import arithmetic.{ComplexLUT, ComplexLUTConfig}
import ip.convenc.{ConvEnc, ConvEncConfig}
import ip.fft.{PeaseFft, PeaseFftConfig}

import spinal.core._
import spinal.lib._

import scala.util.Random

case class OFDMConfig(fold: Int) extends TransformConfig {

  val convConfig = ConvEncConfig(Seq(Seq("171", "133")))
  val spConfig = StridePermutationFor2Config(8, 8 - log2Up(fold), 4, 1)
  val qamConfig = ComplexLUTConfig(Random.RandomComplexSequences(1, 16).head, HardType(SFix(0 exp, -15 exp)))
  val ifftConfig = PeaseFftConfig(64, 2, 16, 12, inverse = true, fold, 1)

  override def inputFlow = CyclicFlow(128 / fold, fold)

  override def outputFlow = CyclicFlow(64 / fold, fold)

  override def latency = 1

  override def implH = OFDM(this)

  // TODO: reference model
}

case class OFDM(config: OFDMConfig) extends TransformModule[Bool, ComplexFix] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bool(), 128 / fold))
  override val dataOut = master Flow Fragment(Vec(ComplexFix(4 exp, -11 exp), 64 / fold))

  val convCores = Seq.fill(128)(convConfig.implH)
  val intrlvCore = StridePermutationFor2(spConfig)
  val qamCores = Seq.fill(64)(ComplexLUT(qamConfig))
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