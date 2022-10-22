package org.datenlord
package flowConverters

import org.jgrapht.alg.color.SmallestDegreeLastColoring
import org.jgrapht.graph._
import spinal.core._

import scala.collection.JavaConversions._


/** control signals applied on each switch of a Benes network
 *
 * @param value 2-D array of control signals, each row contains the control signals of a stage
 */
case class BenesControl[TControl](value: Seq[Seq[TControl]]) {

  val stageCount = value.length
  val switchCountPerStage = value.head.length
  val dataCount = switchCountPerStage * 2
  val switchCount = stageCount * switchCountPerStage

  def getHead: TControl = value.head.head

  def getFirstStage: Seq[TControl] = value.head

  def getLastStage: Seq[TControl] = value.last

  def getMidStages: Seq[Seq[TControl]] = value.drop(1).dropRight(1)

  def getSubUp = BenesControl(getMidStages.map(_.take(switchCountPerStage / 2)))

  def getSubBottom = BenesControl(getMidStages.map(_.takeRight(switchCountPerStage / 2)))

}

case class Permutation(permuted: Seq[Int]) {

  val size = permuted.size
  require(permuted.sorted.equals(permuted.indices))

  def permute[T](dataIn: Seq[T]): Seq[T] = permuted.map(dataIn.apply)

  /** concatenation of permutations
   */
  def concat(that: Permutation) = {
    require(this.size == that.size)
    Permutation(that.permute(permuted))
  }
}

/** model of Benes network
 * @see [[BenesNetworkTest]] for test
 */
object Benes {

  def getUp[T](dataIn: Seq[T]): Seq[T] = dataIn.take(dataIn.size / 2)

  def getBottom[T](dataIn: Seq[T]): Seq[T] = dataIn.takeRight(dataIn.size / 2)

  def doBenes[TData, TControl](dataIn: Seq[TData], controlIn: BenesControl[TControl], butterfly:(TData, TData, TControl) => Seq[TData]): Seq[TData] = {

    def doStage(dataIn: Seq[TData], stageControl: Seq[TControl]): Seq[TData] = {
      val switched = getUp(dataIn).zip(getBottom(dataIn)).zip(stageControl)
        .map { case ((a, b), switch) => butterfly(a, b, switch) } // switches
      switched.map(_.head) ++ switched.map(_.last) // connections
    }

    val n = dataIn.size
    if (n == 2) butterfly(dataIn(0), dataIn(1), controlIn.getHead)
    else {
      // decompose control
      val (pre, post) = (controlIn.getFirstStage, controlIn.getLastStage)
      val (subNetworkUp, subNetworkBottom) = (controlIn.getSubUp, controlIn.getSubBottom)
      // build network recursively
      val afterPreOrdered = doStage(dataIn, pre) // pre-network
      val afterSub = doBenes(getUp(afterPreOrdered), subNetworkUp, butterfly) ++ doBenes(getBottom(afterPreOrdered), subNetworkBottom, butterfly) // recursive build
      doStage(afterSub, post) // post-network
    }
  }

  /** --------
   * software implementation
   -------- */
  def butterfly(a: Int, b: Int, switch: Boolean): Seq[Int] = mux(switch, Seq(b, a), Seq(a, b))
  def doBenes(dataIn: Seq[Int], controlIn: BenesControl[Boolean]): Seq[Int] = doBenes(dataIn, controlIn, butterfly)

  def permutation2Control(permutation: Permutation): BenesControl[Boolean] = {

    val permuted = permutation.permuted
    require(isPow2(permutation.size))
    val n = permuted.size

    if (n == 2) { // base solution
      BenesControl(Seq(Seq(permuted.head == 1))) // for 0,1 -> false, else(1,0) true
    } else {

      /** --------
       * solve current stage problem as by coloring a graph
       * -------- */
      val colorGraph = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
      permuted.indices.foreach(colorGraph.addVertex) // vertices
      // add constraints by adding edges(connected vertices shouldn't have the same color)
      (0 until n / 2).foreach(i => colorGraph.addEdge(i, i + n / 2)) // input side constraint
      (0 until n / 2).foreach(i => colorGraph.addEdge(permuted(i), permuted(i + n / 2))) // output side constraint

      // TODO: find best algo for coloring in this problem
      val color: Seq[(Int, Integer)] = new SmallestDegreeLastColoring(colorGraph).getColoring.getColors.toSeq // solve the problem, get pairs of vertex->color
      require(color.forall(_._2 < 2), s"there are ${color.map(_._2).max + 1} > 2 colors in solution") // 2-color requirement

      /** --------
       * solution extraction
       * -------- */
      val up = color.filter(_._2 == 0).map(_._1) // vertices colored 0
      val bottom = color.filter(_._2 == 1).map(_._1) // vertices colored 1
      val solutionPre: Seq[Boolean] = up.sortBy(_ % (n / 2)).map(_ >= (n / 2))
      val solutionPost: Seq[Boolean] = up.map(permuted.indexOf(_)).sortBy(_ % (n / 2)).map(_ >= (n / 2))

      /** --------
       * sub-problem construction
       * -------- */
      val problem0 = up.sortBy(permuted.indexOf(_) % (n / 2)).map(_ % (n / 2))
      val problem1 = bottom.sortBy(permuted.indexOf(_) % (n / 2)).map(_ % (n / 2))
      val solutionMid: Seq[Seq[Boolean]] =
        permutation2Control(Permutation(problem0)).value // upper sub-network
          .zip(permutation2Control(Permutation(problem1)).value) // low sub-network
          .map { case (s0, s1) => s0 ++ s1 }

      BenesControl(solutionPre +: solutionMid :+ solutionPost)
    }
  }

  def getControlForSim(permutation: Permutation) =
    permutation2Control(permutation).value.map { bools =>
      val bits = bools.map(bool => if (bool) 1 else 0).reverse.mkString("")
      BigInt(bits, 2)
    }

  def getControlForHard(permutation: Permutation) =
    Vec(getControlForSim(permutation).map(B(_, permutation.size / 2 bits)))
}