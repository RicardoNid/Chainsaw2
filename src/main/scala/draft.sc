import optimus.algebra.Constraint
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import collection.mutable
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar

import scala.collection.mutable.ArrayBuffer


def getNewSplits(widthsOfMerge:Seq[Int], widthsOfSplit: Seq[Int]) = {

  val mergeSplits = widthsOfMerge.reverse.scan(0)(_ + _).init
  val splitSplits = widthsOfSplit.reverse.scan(0)(_ + _).init
  val splitPoints = (mergeSplits ++ splitSplits).sorted.distinct

  // split schemes of inputs, low -> high
  val mergeBuffer = Seq.fill(widthsOfMerge.length)(ArrayBuffer[Int]())

  splitPoints.foreach { p =>
    val index = mergeSplits.lastIndexWhere(_ <= p) // find last where p >= value
    val value = mergeSplits(index)
    if((p - value) > 0) mergeBuffer(index) += (p - value)
  }

  // merge schemes of outputs
  val splitBuffer = Seq.fill(widthsOfSplit.length)(ArrayBuffer[Int]())
  splitPoints.zipWithIndex.foreach { case(p, index) =>
    println(s"p -> $p")
    val group = splitSplits.lastIndexWhere(_ <= p) // find last where p >= value
    splitBuffer(group) += index
  }

  (mergeBuffer, splitBuffer)
}

getNewSplits(Seq(1,1,4,4), Seq(2,4,4))
getNewSplits(Seq(3,3,3,3), Seq(4,4,4))




