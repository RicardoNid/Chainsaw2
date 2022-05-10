package org.datenlord
package ip.ofdm

import dataFlow._
import ip.convenc.Convenc128FTN
import ip.fft.{PeaseFft, PeaseFftConfig}

import spinal.core._
import spinal.lib._

case class OFDMConfig(fold: Int) extends TransformConfig {
  override def latency = ???

  val spConfig = StridePermutation2Config(128, 128 / fold, 16, 1)
  val ifftConfig = PeaseFftConfig(64, 2, 16, 12, inverse = true, fold, 1)

  override def inputFlow = CyclicFlow(128 / fold, fold)

  override def outputFlow = CyclicFlow(64 / fold, fold)
}

case class OFDM(config: OFDMConfig) extends TransformModule[Bool, ComplexFix] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bool(), 128 / fold))
  override val dataOut = master Flow Fragment(Vec(ComplexFix(3 exp, -12 exp), 64 / fold))

  val convCore = Convenc128FTN()
  val intrlvCore = StridePermutation2(spConfig)
  val ifftCore = PeaseFft(ifftConfig)

  convCore.dataIn := dataIn.fragment.asBits
  val afterConv = convCore.dataOut
  intrlvCore.dataIn.fragment := Vec(afterConv.asBools.map(B(_)))
  val afterIntrlv = intrlvCore.dataOut.fragment
//  val





}