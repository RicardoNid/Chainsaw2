package org.datenlord
package arithmetic

import org.datenlord.TransformDfg
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/** do modular reduction for an input very close to [0, M) (diff < 20M)
 *
 * @param M          the modulus
 * @param upperBound the input falls in [0, upperBound * M)
 */
case class FineReduction(M: BigInt, upperBound: Int) extends TransformDfg {

  // TODO: support upperbound + lower bound
  // TODO: optimize for upperBound = 2

  require(upperBound <= 20, "for input > ")
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
  logger.info(s"det table:\n ${detTable.zipWithIndex.map { case (value, i) => s"$i -> $value" }.mkString("\n")}")
  val sub0Config, sub1Config = CpaConfig(widthIn + 1, BinarySubtractor, sub = 1)

  override def latency = if (upperBound == 2) sub0Config.latency + 1 else sub0Config.latency + 4

  val transformConfig = this

  override def implH: TransformModule[UInt, UInt] = new TransformModule[UInt, UInt] {
    override val config = transformConfig
    override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), 1))
    override val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), 1))

    if (skipComponentSim) {
      dataOut.fragment.head := (dataIn.fragment.head % M).resize(k).d(latency)
    } else {
      if (upperBound == 2) {
        val T = dataIn.payload.head
        val diff = sub0Config.asFunc(Seq(T, U(M, k + 1 bits))).head
        dataOut.fragment.head := Mux(diff.msb, T.d(sub0Config.latency).resize(k), diff.resize(k)).d(1)
      }
      else {
        val multipleCount = detTable.distinct.length // number of different multiples needed
        val detRom = Mem(detTable.map(U(_, log2Up(multipleCount) bits)))
        val multipleRom = Mem((0 to detTable.max.toInt).map(value => U(M * value, widthIn bits)))

        val T = dataIn.payload.head
        val det = T.takeHigh(detWidth).asUInt

        val multipleAddr = detRom.readSync(det)
        val multiple0 = multipleRom.readSync(multipleAddr).d(1) // \widetilde{Y}M
        val multiple1 = multipleRom.readSync(multipleAddr + 1).d(1) // (\widetilde{Y}+1)M
        val ret0 = sub0Config.asFunc(Seq(T.d(3), multiple0)).head // d(3) for readSync latency
        val ret1 = sub1Config.asFunc(Seq(T.d(3), multiple1)).head

        dataOut.fragment.head := Mux(ret1.msb, ret0, ret1).d(1).resize(widthOut)
      }
    }
    autoValid()
    autoLast()
  }
}


