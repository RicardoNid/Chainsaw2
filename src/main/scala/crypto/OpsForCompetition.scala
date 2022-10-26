package org.datenlord
package crypto

import org.datenlord.flowConverters._
import spinal.core._

import scala.util.Random


object ModularMult96 extends ChainsawGenerator {
  override def name = "montMult96"

  val modulus = BigInt(96, Random)

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = Seq(dataIn.asInstanceOf[Seq[BigInt]].product % modulus)

  override var inputTypes = Seq.fill(2)(UIntInfo(96))
  override var outputTypes = Seq.fill(1)(UIntInfo(96))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = 15

  override def implH = null

  override def implNaiveH: Option[ChainsawModule] = Some(new ChainsawModule(this) {

    uintDataOut.head := (uintDataIn.reduce(_ * _) % U(modulus, 96 bits)).d(latency)

  })
}

case class Permutation(row: Int, col: Int, sw: Int, bw: Int) extends ChainsawGenerator {
  override def name = s"perm"

  val frameLength = 1 << (row + col)
  val p = 1 << (row + col - sw)

  override def impl(dataIn: Seq[Any]) = dataIn.grouped(1 << col).toSeq.transpose.flatten

  override var inputTypes = Seq.fill(1 << sw)(UIntInfo(bw))
  override var outputTypes = Seq.fill(1 << sw)(UIntInfo(bw))

  override var inputFormat = MatrixFormat(1 << sw, p)
  override var outputFormat = MatrixFormat(1 << sw, p)
  val s2pGen = S2P(1 << sw, frameLength, bw)
  val p2sGen = P2S(frameLength, 1 << sw, bw)
  override var latency = s2pGen.latency + p2sGen.latency

  override def implH = null

  override def implNaiveH: Option[ChainsawModule] = Some {
    new ChainsawModule((this)) {

      // control
      val s2p = s2pGen.implH
      val p2s = p2sGen.implH
      s2p.dataIn := dataIn
      s2p.lastIn := lastIn

      p2s.dataIn := s2p.dataOut.grouped(1 << col).toSeq.transpose.flatten
      val lastForP2S = lastIn.validAfter(s2pGen.latency)
      p2s.lastIn := lastForP2S

      dataOut := p2s.dataOut
    }
  }
}