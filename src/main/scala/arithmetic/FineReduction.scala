package org.datenlord
package arithmetic

import org.datenlord.TransformDfg
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class FineReduction(M: BigInt, upperBound: Int) extends TransformDfg {

  override val name = "FineReduction"
  override val opType = Custom

  val k = M.bitLength
  val widthIn = log2Up(upperBound) + k
  val widthOut = k

  override val widthsIn = Seq(widthIn)

  override val widthsOut = Seq(widthOut)

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].head % M)

  override val size = (1, 1)

  val detWidth = log2Up(upperBound) + 1 // downto k-1
  val detTable = (0 until 1 << detWidth).map(msbValue => (BigInt(msbValue) << (k - 1)) / M)
  logger.info(s"det table:\n ${detTable.zipWithIndex.map{case(value, i) => s"$i -> $value" }.mkString("\n")}")
  val sub0Config, sub1Config = CpaConfig(widthIn + 1, BinarySubtractor, sub = 1)

  override def latency = sub0Config.latency + 1

  val transformConfig = this
  override def implH: TransformModule[UInt, UInt] = new TransformModule[UInt, UInt] {
    override val config = transformConfig
    override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), 1))
    override val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), 1))

    val multipleRom0 = Mem(detTable.map(value => U(M * value, widthIn bits))) // \widetilde{Y}M
    val multipleRom1 = Mem(detTable.map(value => U(M * (value + 1), widthIn bits))) // (\widetilde{Y}+1)M

    val T = dataIn.payload.head
    val det = T.takeHigh(detWidth).asUInt
    // TODO: reduce rom size
    val multiple0 = multipleRom0.readSync(det)
    val multiple1 = multipleRom1.readSync(det)
    val ret0 = sub0Config.asFunc(Seq(T.d(1), multiple0)).head // d(1) for readSync latency
    val ret1 = sub1Config.asFunc(Seq(T.d(1), multiple1)).head
    dataOut.fragment.head := Mux(ret1.msb, ret0, ret1).resize(widthOut)
    autoValid()
    autoLast()
  }
}


