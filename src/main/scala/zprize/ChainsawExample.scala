package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import zprize.ChainsawGenerator

import scala.language.postfixOps
import scala.util.Random

// a simple ChainsawGenerator
case class ChainsawAddGen(width: Int) extends ChainsawGenerator {

  override val name = s"adder_$width"
  override val impl = (dataIn: Seq[Any]) => Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)
  override var inputWidths = Seq.fill(2)(width)
  override var outputWidths = Seq(width + 1)
  override val frameFormat = frameNoControl
  override val inputType = HardType(UInt())
  override val outputType = HardType(UInt())
  override var latency = 1

  override def implH: ChainsawModule = new ChainsawModule(this) {
    dataOut.head := dataIn.map(_.asUInt).reduce(_ +^ _).d(1).asBits
  }
}

// a more complex ChainsawGenerator with configurable I/O
case class CpaGen(width: Int, mode: Int) extends ChainsawGenerator {

  override val name = "cpa"
  override val impl = (dataIn: Seq[Any]) => Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)

  override var inputWidths = mode match {
    case 0 => Seq(4 * width)
    case 1 => Seq.fill(width)(4)
    case 2 => Seq.fill(width)(4)
  }

  override var outputWidths = mode match {
    case 0 => Seq.fill(width)(4)
    case 1 => Seq.fill(width)(4)
    case 2 => Seq(4 * width)
  }

  override val frameFormat = frameNoControl

  override val inputTimes = mode match {
    case 0 => Seq(0)
    case 1 => Seq(0, 1, 2, 3)
    case 2 => Seq(0, 1, 2, 3)
  }

  override val outputTimes = mode match {
    case 0 => Seq(0, 1, 2, 3)
    case 1 => Seq(0, 1, 2, 3)
    case 2 => Seq(0)
  }

  override val inputType = HardType(UInt())
  override val outputType = HardType(UInt())
  override var latency = if (mode == 2) 4 else 1

  override def implH: ChainsawModule = null
}

object ChainsawExample extends App {

  // the generator can be instantiated outside the SpinalHDL context
  val generator = ChainsawAddGen(10)

  // warning: generator already exist
  val generatorAnother = ChainsawAddGen(10)

  def bigger0 = new Module {
    val a, b, c, d = in UInt (10 bits)
    val y, z = out UInt (11 bits)
    y := generator.asFunc(Seq(a, b).map(_.asBits)).head.asUInt
    z := generator.asFunc(Seq(c, d).map(_.asBits)).head.asUInt
  }

  val generatorPlus1 = ChainsawAddGen(11)

  // different generator instantces lead to multiple .v files
  def bigger1 = new Module {
    val a, b = in UInt (10 bits)
    val c, d = in UInt (11 bits)
    val y = out UInt (11 bits)
    val z = out UInt (12 bits)
    y := generator.asFunc(Seq(a, b).map(_.asBits)).head.asUInt
    z := generatorPlus1.asFunc(Seq(c, d).map(_.asBits)).head.asUInt
  }

  // a simple Dag
  case class AdderGraph() extends Dag {
    override val name = "adderGraph"
    override val impl = (dataIn: Seq[Any]) => Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)
    override val inputType = HardType(UInt())
    override val outputType = HardType(UInt())
    override val frameFormat = frameNoControl
    override val inputTimes = Seq.fill(4)(0)
    override val outputTimes = Seq(0)

    // declare components
    // declare IO
    val i0, i1, i2, i3 = InputVertex(10)
    val o = OutputVertex(12)
    // declare submodules
    val v0, v1 = generator.asVertex
    val v2 = generatorPlus1.asVertex

    v0.in(0) := i0
    v0.in(1) := i1
    v1.in(0) := i2
    v1.in(1) := i3
    v2.in(0) := v0.out(0)
    v2.in(1) := v1.out(0)
    o := v2.out(0)

    updateHardwareData() // this must be invoked explicitly
  }

  case class CpaGraph() extends Dag {
    override val name = "cpaGraph"
    override val impl = (dataIn: Seq[Any]) => Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)
    override val inputType = HardType(UInt())
    override val outputType = HardType(UInt())
    override val frameFormat = frameNoControl
    override val inputTimes = Seq(0)
    override val outputTimes = Seq(0)

    // declare components
    // declare IO
    val i = InputVertex(40)
    val o = OutputVertex(40)
    // declare submodules

    val widths = Seq.fill(4)(10)
    val add0 = Cpa(BinaryAdder, 0, widths, S2M).asVertex // aligned -> diff
    val add1 = Cpa(BinaryAdder, 0, widths, M2M).asVertex // diff -> diff
    val add2 = Cpa(BinaryAdder, 0, widths, M2S).asVertex // diff -> aligned

    //    val s2s = Cpa(BinaryAdder, 0, widths, S2S)
    //    val add0 = s2s.asVertex // aligned -> diff
    //    val add1 = s2s.asVertex // diff -> diff
    //    val add2 = s2s.asVertex // diff -> aligned

    add0.in(0) := i
    add1.in(0) := add0.out(0)
    add1.in(1) := add0.out(1)
    add1.in(2) := add0.out(2)
    add1.in(3) := add0.out(3)

    add2.in(0) := add1.out(0)
    add2.in(1) := add1.out(1)
    add2.in(2) := add1.out(2)
    add2.in(3) := add1.out(3)
    o := add2.out(0)

    updateHardwareData() // this must be invoked explicitly
  }

  verbose = 1

  //  RtlGen(generator.implH)
  // this implementation consumes 11 + 2 FFs, 2 FF for valid & last
  //  VivadoSynth(generator.implH)

  // RtlGen(bigger0, "bigger0")
  // this implementation consumes 11 * 2 FFs, which means that valid & last paths are successfully pruned
  // code of the module will appear once and only once in code generated, as SpinalHDL found same layouts in two instances
  //  VivadoSynth(bigger0)

  //  RtlGen(bigger1, "bigger1")
  //  VivadoSynth(bigger1)

  // this example show the way you construct a Dag
  // println(AdderGraph())
  // println(AdderGraph().autoPipeline().retimingInfo.mkString("\n"))

  val adderGraph = AdderGraph()
  RtlGen(adderGraph.implH, "adder")
  RtlGen(adderGraph.implDut, "adderDut")

  //  println(CpaGraph().autoPipeline().retimingInfo.mkString("\n"))
  CpaGraph().autoPipeline().toPng()

  //  val cpaExample = Cpa(BinaryAdder, 0, Seq.fill(4)(10), M2M)
  //  RtlGen(cpaExample.implH, "cpa")
  //  RtlGen(cpaExample.implDut, "cpaDut")

  // this example show the way a graph implement itself

}
