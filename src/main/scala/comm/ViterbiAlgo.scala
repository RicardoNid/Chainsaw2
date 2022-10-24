package org.datenlord
package comm

import spinal.core._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map, Queue, Stack}
import com.mathworks.matlab.types._
import MinplusMatrix.max

/**
 * @param numInputSymbols  number of distinct input symbols
 * @param numOutputSymbols number of distinct output symbols
 * @param numStates        number of states
 * @param nextStates       2-D matrix of (state, input) -> nextState
 * @param outputs          2-D matrix of (state, input) -> output
 * @see ''Matlab istrellis & poly2trellis''
 * @see ''Coding Theory'' Chap3.2.2, symbols n,k,m are defined in Chap3.1.2
 */
case class Trellis(numInputSymbols: Int, numOutputSymbols: Int, numStates: Int, nextStates: Array[Array[Int]], outputs: Array[Array[Int]]) {

  val inputBitWidth = log2Up(numInputSymbols)
  val outputBitWidth = log2Up(numOutputSymbols)
  val stateBitWidth = log2Up(numStates)

  require(nextStates.length == numStates && nextStates.head.length == numInputSymbols)
  require(outputs.length == numStates && outputs.head.length == numInputSymbols)

  val lookBackMap = Map[Int, ArrayBuffer[(Int, Int)]]() // lookBackMap(nextState) gets the sequence of (prevState, output)
  Seq.tabulate(numInputSymbols, numStates) { (inputSymbol: Int, prevState: Int) =>
    val nextState = nextStates(prevState)(inputSymbol)
    val outputSymbol = outputs(prevState)(inputSymbol)
    if (lookBackMap.isDefinedAt(nextState)) lookBackMap(nextState) += Tuple2(prevState, outputSymbol)
    else lookBackMap += nextState -> ArrayBuffer(Tuple2(prevState, outputSymbol))
  }

  def getPrevStates(state: Int): Seq[Int] = lookBackMap(state).map(_._1)

  def getOutputs(state: Int): Seq[Int] = lookBackMap(state).map(_._2)

  def getHamming(expected: Int, observed: Int): Int = (expected ^ observed).toBinaryString.map(_.asDigit).sum

  def toMinplus: Seq[MinplusMatrix] = {
    (0 until numOutputSymbols).map { observed =>
      val values = Array.tabulate(numStates, numStates) { (prev, next) =>
        val index = nextStates(prev).indexOf(next)
        if (index == -1) max
        else getHamming(outputs(prev)(index), observed)
      }
      MinplusMatrix(values)
    }
  }
}

object Trellis {

  /** generate trellis from a convolutional code configuration
   */
  def apply(constLen: Int, codeGen: Array[Int]): Trellis = {
    val matlabTrellis = matlabEngine.feval[Struct]("poly2trellis", Array(constLen.toDouble), codeGen.map(_.toDouble))
    Trellis(
      numInputSymbols = matlabTrellis.get("numInputSymbols").asInstanceOf[Double].toInt,
      numOutputSymbols = matlabTrellis.get("numOutputSymbols").asInstanceOf[Double].toInt,
      numStates = matlabTrellis.get("numStates").asInstanceOf[Double].toInt,
      nextStates = matlabTrellis.get("nextStates").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt)),
      outputs = matlabTrellis.get("outputs").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt))
    )
  }
}

case class MinplusMatrix(value: Array[Array[Int]]) {

  def rows: Int = value.length

  def cols: Int = value.head.length

  def apply(i: Int, j: Int) = this.value(i)(j)

  def toDense: Array[Int] = value.flatten.filter(_ < MinplusMatrix.max)

  def isDense = value.forall(_.forall(_ < MinplusMatrix.max))

  override def toString = s"Minplus matrix: \n${this.value.map(_.map(i => if (i >= max) "-" else i.toString).mkString(" ")).mkString("\n")}"

  def *(that: MinplusMatrix): MinplusMatrix = {
    require(this.cols == that.rows)
    val ret = Array.tabulate(this.rows, that.cols) { (i, k) =>
      (0 until this.cols).map(j => this (i, j) + that(j, k)).min // min-plus
    }
    MinplusMatrix(ret)
  }
}

object MinplusMatrix {

  def max = Int.MaxValue / 3

}

object ViterbiAlgo {

  def viterbiForwarding(observed: Array[Int], trellis: Trellis, stateStart: Int = 0, max: Int = MinplusMatrix.max): Stack[Seq[Int]] = {

    import trellis._

    val records = Stack[Seq[Int]]()
    val observedQueue = Queue(observed: _*)
    val minplusMatrices = trellis.toMinplus

    // initialization
    val vector: Seq[Int] = (0 until numStates).map(currentState => if (currentState == stateStart) 0 else max)
    var currentDiscrepancies = MinplusMatrix(Array(vector.toArray))
    records.push(vector)

    // writing records iteratively
    while (observedQueue.nonEmpty) {
      val currentObserved = observedQueue.dequeue()
      val updatingMatrix = minplusMatrices(currentObserved)
      currentDiscrepancies = currentDiscrepancies * updatingMatrix
      records.push(currentDiscrepancies.value.head)
    }

    records.pop() // when the final stage is known, this won't be used
    records
  }

  /**
   * @return states
   */
  def viterbiBackwarding(records: Stack[Seq[Int]], trellis: Trellis): Array[Int] = {

    import trellis._

    // starts from minimum
    val decoded = ArrayBuffer[Int]()
    var currentState = 0
    decoded += currentState
    // iteratively tracing back
    while (records.nonEmpty) {
      val prevData = lookBackMap(currentState)
      val discrepancies = records.pop()
      val prevState = prevData.minBy { case (state, output) => discrepancies(state) }._1 // TODO: explain why this logic(not the line above)
      currentState = prevState
      decoded += currentState
    }

    decoded.reverse.toArray
  }

  def viterbiTraceback(observed: Array[Int], trellis: Trellis, stateStart: Int = 0) = {
    val records = viterbiForwarding(observed, trellis, stateStart)
    val states = viterbiBackwarding(records, trellis)
    states.tail.map(_.toBinaryString.padToLeft(trellis.stateBitWidth, '0').head.asDigit)
  }

}