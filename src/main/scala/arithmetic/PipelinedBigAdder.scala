package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.math.ceil

import dfg._

// TODO: minus
// TODO: use "computer arithmetic" as reference
/**
 * @param addWidt`h
 * @param baseWidth
 */
case class PipelinedBigAdderConfig(addWidth: Int, baseWidth: Int = 127, minus: Boolean = false) extends TransformBase {

  val graph = dfg.ArithmeticGraphs.addGraph(addWidth, baseWidth).validate(100)

  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    if (!minus) Seq(bigInts.sum)
    else {
      assert(bigInts.head > bigInts.last, s"this module is for UInt and thus works only when x > y, for x - y")
      Seq(bigInts.head - bigInts.last)
    }
  }

  override val size = (2, 1)

  override def latency = graph.latency

  override def implH = PipelinedBigAdder(this)

  def asOperator = (x: UInt, y: UInt) => {
    val core = implH
    core.dataIn.fragment := Vec(x.resized, y.resized)
    core.skipControl()
    core.dataOut.fragment.head
  }
}

case class PipelinedBigAdder(config: PipelinedBigAdderConfig) extends TransformModule[UInt, UInt] {

  import config._

  val N = ceil(addWidth.toDouble / baseWidth).toInt

  val dataIn = slave Flow Fragment(Vec(UInt(addWidth bits), 2))
  val dataOut = master Flow Fragment(Vec(UInt(addWidth + 1 bits)))

  val hardAlgo = graph.implH
  dataOut.fragment := Vec(hardAlgo(dataIn.fragment))

  autoValid()
  autoLast()
}