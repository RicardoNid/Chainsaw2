/** --------
 * general purpose compressors
 * -------- */

package org.datenlord
package arithmetic

import device._
import org.datenlord.xilinx.{VivadoUtil, VivadoUtilRequirement}
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** providing a list of available compressors
 *
 * @see [[Compressor4to2]]
 * @see [[Compressor3to1]]
 * @see [[Compressor3to2]]
 * @see [[Compressor6to3]]
 */
object Gpcs {
  def apply(): Seq[Compressor[Bool]] = {
    val ret = Seq(Compressor1to1, Compressor4to2, Compressor3to1, Compressor6to3, Compressor3to2)
    ret
  }
}

/** general parallel counter (4; 2) for Xilinx FPGA
 *
 * @see ''Kumm, Martin & Zipf, P.. (2014). Efficient High Speed Compression Trees on Xilinx FPGAs. ''
 * @see ''Parhami, Behrooz. “Computer arithmetic - algorithms and hardware designs.” (2010).'' 8.4 PARALLEL COUNTERS AND COMPRESSORS
 */
case class Compressor4to2Hard(width: Int) extends Component {
  val cIn = in Bool()
  val w, x, y, z = in UInt (width bits)
  val sumsOut, carrysOut = out UInt (width bits)
  val cOut = out Bool()

  def lut(w: Bool, x: Bool, y: Bool, z: Bool) = {
    val core = LUT6_2(BigInt("69966996e8e8e8e8", 16))
    core.I0 := x
    core.I1 := y
    core.I2 := z
    core.I3 := w
    core.I4 := False
    core.I5 := True
    (core.O5, core.O6) // O5 is carry output, O6 is XOR output
  }

  val lutOuts = (0 until width).map(i => lut(w(i), x(i), y(i), z(i)))

  val carryCount = (width + 7) / 8
  val carryChains = Seq.fill(carryCount)(CARRY8())
  val selects = lutOuts.map(_._2)
  val data = w.asBits

  carryChains.zipWithIndex.foreach { case (carryChain, i) =>
    (0 until 8).foreach { j =>
      val index = i * 8 + j
      if (index < width) {
        carryChain.DI(j) := data(index)
        carryChain.S(j) := selects(index)
      }
      else {
        carryChain.DI(j) := False
        carryChain.S(j) := False
      }
    }
    if (i == 0) carryChain.CI := cIn else carryChain.CI := carryChains(i - 1).CO(7)
    carryChain.CI_TOP := False
  }

  carrysOut := lutOuts.map(_._1).asBits().asUInt
  sumsOut := carryChains.reverse.map(_.O).reduce(_ @@ _).takeLow(width).asUInt
  cOut := carryChains.last.CO((width + 7) % 8)
}

/**
 * @see ''''Kumm, Martin & Zipf, P.. (2014). Efficient High Speed Compression Trees on Xilinx FPGAs. ''''
 */
case class Compressor3to1Hard(width: Int, sub: Int = 0) extends Component {
  val cIn0, cIn1 = in Bool()
  val x, y, z = in UInt (width bits)
  val sumsOut = out UInt (width bits)
  val cOut0, cOut1 = out Bool()

  val lutContent = sub match {
    case 0 => BigInt("69699696e8e8e8e8", 16) // x + y + z + cin0 + cin1
    case 1 => BigInt("969669698e8e8e8e", 16) // x + y - z - 1 + cin0 + cin1
  }

  def lut(c: Bool, x: Bool, y: Bool, z: Bool) = {
    val core = LUT6_2(lutContent)
    core.I0 := x
    core.I1 := y
    core.I2 := z
    core.I3 := False
    core.I4 := c
    core.I5 := True
    (core.O5, core.O6) // O5 is carry output, O6 is XOR output
  }

  val innerCarries = Seq.fill(width + 1)(Bool())

  val lutOuts = (0 until width).map(i => lut(innerCarries(i), x(i), y(i), z(i)))
  innerCarries.head := cIn1
  innerCarries.tail.zip(lutOuts.map(_._1)).foreach { case (port, signal) => port := signal }
  cOut1 := innerCarries.last

  val carryCount = (width + 7) / 8
  val carryChains = Seq.fill(carryCount)(CARRY8())

  carryChains.zipWithIndex.foreach { case (carryChain, i) =>
    (0 until 8).foreach { j =>
      val index = i * 8 + j
      if (index < width) {
        carryChain.DI(j) := innerCarries(index)
        carryChain.S(j) := lutOuts(index)._2
      } else {
        carryChain.DI(j) := False
        carryChain.S(j) := False
      }
    }
    if (i == 0) carryChain.CI := cIn0 else carryChain.CI := carryChains(i - 1).CO(7)
    carryChain.CI_TOP := False
  }

  sumsOut := carryChains.reverse.map(_.O).reduce(_ @@ _).takeLow(width).asUInt
  cOut0 := carryChains.last.CO((width + 7) % 8)
}

object Compressor4to2 extends Compressor[Bool] {
  override val isFixed = false

  override val widthMax = 32

  override val widthMin: Int = 1

  override def inputFormat(width: Int) = Seq.fill(width)(4)

  override def outputFormat(width: Int) = 1 +: Seq.fill(width)(2)

  override def cost(width: Int): Int = width

  override def impl(bitsIn: BitHeap[Bool], width: Int) = {
    val Seq(w, x, y, z) = bitsIn.bitHeap.map(_.padTo(4, False)).transpose.map(_.asBits().asUInt)
    val core = Compressor4to2Hard(width)
    core.cIn := False
    core.w := w
    core.x := x
    core.y := y
    core.z := z
    val bitHeap = ArrayBuffer.fill(width + 1)(ArrayBuffer[Bool]())
    bitHeap.last += core.cOut
    core.sumsOut.asBools.zip(bitHeap).foreach { case (bit, column) => column += bit }
    core.carrysOut.asBools.zip(bitHeap.tail).foreach { case (bit, column) => column += bit }
    BitHeap(bitHeap, bitsIn.weightLow)
  }

  override def utilRequirement(width: Int) = VivadoUtilRequirement(lut = width, carry8 = width.divideAndCeil(8))

  override def fMaxRequirement: HertzNumber = 600 MHz
}

/** compression by ternary adder
 *
 */
object Compressor3to1 extends Compressor[Bool] {

  override val isFixed = false

  override val widthMax = 16

  override val widthMin = 1

  override def inputFormat(width: Int): Seq[Int] = 5 +: Seq.fill(width - 1)(3)

  override def outputFormat(width: Int): Seq[Int] = Seq.fill(width)(1) :+ 2

  override def cost(width: Int): Int = width

  override def impl(bitsIn: BitHeap[Bool], width: Int) = {

    val paddedHeap = bitsIn.bitHeap.head.padTo(5, False) +: bitsIn.bitHeap.tail.map(_.padTo(3, False))
    val Seq(cIn0, cIn1) = paddedHeap.head.takeRight(2)
    val Seq(x, y, z) = (paddedHeap.head.take(3) +: paddedHeap.tail).transpose.map(_.asBits().asUInt)
    val core = Compressor3to1Hard(width)
    core.cIn0 := cIn0
    core.cIn1 := cIn1

    core.x := x
    core.y := y
    core.z := z

    val bitHeap = ArrayBuffer.fill(width + 1)(ArrayBuffer[Bool]())
    bitHeap.last += core.cOut0
    bitHeap.last += core.cOut1
    core.sumsOut.asBools.zip(bitHeap).foreach { case (bit, column) => column += bit }
    BitHeap(bitHeap, bitsIn.weightLow)
  }

  override def utilRequirement(width: Int) = VivadoUtilRequirement(lut = width, carry8 = width.divideAndCeil(8))

  override def fMaxRequirement: HertzNumber = 600 MHz
}

object Compressor1to1 extends Compressor[Bool] {

  override val isFixed = false

  override val widthMax = Int.MaxValue

  override val widthMin: Int = 1

  override def inputFormat(width: Int) = Seq.fill(width)(1)

  override def outputFormat(width: Int) = Seq.fill(width)(1)

  override def cost(width: Int): Int = 0

  override def impl(bitsIn: BitHeap[Bool], width: Int): BitHeap[Bool] = bitsIn

  override def utilRequirement(width: Int) = null

  override def fMaxRequirement: HertzNumber = 600 MHz
}

case class Compressor6to3Hard() extends Component {

  val dataIn = in Bits (6 bits)
  val dataOut = out Bits (3 bits)

  // TODO: implement this by LUT primitive
  switch(dataIn) {
    (0 until 64).foreach { i =>
      is(B(i, 6 bits))(dataOut := B(i.toBinaryString.map(_.asDigit).sum, 3 bits))
    }
  }
}

object Compressor6to3 extends Compressor[Bool] {

  override val isFixed = true

  override val widthMax = 1

  override val widthMin: Int = 1

  override def inputFormat(width: Int) = Seq(6)

  override def outputFormat(width: Int) = Seq.fill(3)(1)

  override def cost(width: Int): Int = 3

  override def impl(bitsIn: BitHeap[Bool], width: Int): BitHeap[Bool] = {
    val dataIns = bitsIn.bitHeap.head.padTo(6, False).asBits()
    val core = Compressor6to3Hard()
    core.dataIn := dataIns
    val ret = core.dataOut
    val bitHeap = ArrayBuffer.fill(3)(ArrayBuffer[Bool]())
    ret.asBools.zip(bitHeap).foreach { case (bit, column) => column += bit }
    BitHeap(bitHeap, bitsIn.weightLow)
  }

  override def utilRequirement(width: Int) = VivadoUtilRequirement(lut = 3, carry8 = 0)

  override def fMaxRequirement: HertzNumber = 600 MHz
}

case class Compressor3to2Hard() extends Component {
  val dataIn = in UInt (3 bits)
  val dataOut = out UInt (2 bits)

  def lut(x: Bool, y: Bool, z: Bool) = {
    val core = LUT6_2(BigInt("96969696e8e8e8e8", 16))
    core.I0 := x
    core.I1 := y
    core.I2 := z
    core.I3 := False
    core.I4 := False
    core.I5 := True
    (core.O5, core.O6) // O5 is carry output, O6 is XOR output
  }

  val lutOuts = lut(dataIn(0), dataIn(1), dataIn(2))
  dataOut := (lutOuts._1 ## lutOuts._2).asUInt
}

/** full adder with carry
 *
 */
// FIXME: this compressor is wrong
object Compressor3to2 extends Compressor[Bool] {

  override val isFixed = true

  override val widthMax = 16

  override val widthMin = 1

  override def inputFormat(width: Int) = Seq(3)

  override def outputFormat(width: Int): Seq[Int] = Seq.fill(2)(1)

  override def cost(width: Int): Int = 1

  // TODO: implement this by LUT primitive
  override def impl(bitsIn: BitHeap[Bool], width: Int): BitHeap[Bool] = {
    val dataIns = bitsIn.bitHeap.head.padTo(3, False)
    val ret = Compressor3to2Hard()
    ret.dataIn := dataIns.asBits().asUInt
    val bitHeap = ArrayBuffer.fill(2)(ArrayBuffer[Bool]())
    ret.dataOut.asBools.zip(bitHeap).foreach { case (bit, column) => column += bit }
    BitHeap(bitHeap, bitsIn.weightLow)
  }

  override def utilRequirement(width: Int): VivadoUtil = VivadoUtilRequirement(lut = 1, carry8 = 0)

  override def fMaxRequirement: HertzNumber = 600 MHz
}