package org.datenlord
package arithmetic

import breeze.math._
import spinal.core._
import spinal.lib._

case class LUTConfig[TSoft, THard <: Data](coeffs: Seq[TSoft], dataType: HardType[THard])
  extends TransformBase {

  val inputBitWidth = log2Up(coeffs.length)

  override val size = (1, 1)

  override def latency = 1

  override def impl(dataIn: Seq[Any]) ={
    println(coeffs.mkString(" "))
    Seq(coeffs(dataIn.head.asInstanceOf[BigInt].toInt))
  }

  override def implH = LUT(this)

  override def getConfigWithFoldsChanged(spaceFold: Int, timeFold: Int) = this
}

case class LUT[TSoft, THard <: Data](config: LUTConfig[TSoft, THard]) extends TransformModule[UInt, THard] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(inputBitWidth bits)))
  override val dataOut = master Flow Fragment(Vec(dataType, 1))

  dataType() match {
    case cf: ComplexFix =>
      val sfixType = cf.sfixType
      val coeffHard = coeffs.asInstanceOf[Seq[Complex]].map(CF(_, sfixType))
      val ROM = Mem(coeffHard)
      dataOut.fragment := Vec(ROM.readSync(dataIn.fragment.head).asInstanceOf[THard])
    case sf: SFix =>
      val peak = sf.maxExp
      val resolution = sf.minExp
      val coeffHard = coeffs.asInstanceOf[Seq[Double]].map(SF(_, peak exp, resolution exp))
      val ROM = Mem(coeffHard)
      dataOut.fragment := Vec(ROM.readSync(dataIn.fragment.head).asInstanceOf[THard])
    case si: SInt =>
      val width = si.getBitsWidth
      val coeffHard = coeffs.asInstanceOf[Seq[Int]].map(S(_, width bits))
      val ROM = Mem(coeffHard)
      dataOut.fragment := Vec(ROM.readSync(dataIn.fragment.head).asInstanceOf[THard])
    case ui: UInt =>
      val width = ui.getBitsWidth
      val coeffHard = coeffs.asInstanceOf[Seq[Int]].map(U(_, width bits))
      val ROM = Mem(coeffHard)
      dataOut.fragment := Vec(ROM.readSync(dataIn.fragment.head).asInstanceOf[THard])
  }

  autoValid()
  autoLast()
}