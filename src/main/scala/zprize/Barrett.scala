package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.traverse._
import org.jgrapht.generate._
import scala.collection.JavaConversions._ // as JGraphT is based on Java

case class Barrett(k: Int, M: BigInt) extends Dag {

  override def name = ???

  /** --------
   * golden model
   * -------- */
  override val impl = (dataIn: Seq[Any]) => Seq(dataIn.asInstanceOf[Seq[BigInt]].product % M)

  require(M.bitLength == k)

  val MPrime = (BigInt(1) << (2 * M.bitLength)) / M

  val mult0 = Karatsuba(k).asVertex
  val mult1 = Karatsuba(k + 1, Some(MPrime)).asVertex
  val mult2 = Karatsuba(k + 1, Some(M)).asVertex

  val splitN = Split(k + 1, k - 1).asVertex
  val split1 = Split(k + 1, k + 1).asVertex
  val split2 = Split(k, k + 2).asVertex
  val cpa0 = Cpa(BinarySubtractor, Seq(k + 3), S2S)

  // first multiplication
  val A, B = InputVertex(UIntInfo(k))
  mult0 := (A, B)
  splitN := mult0.out(0)
  mult1 := splitN.out(0)
  split1 := mult1.out(0)
  mult2 := split1.out(0)



  val N = mult0.out(0)
  val NHigh =


  val A = graph.addInput("A", k)
  val B = graph.addInput("B", k)
  val N = fullMult(Seq(A, B)).head // 2k
  // bits
  // second multiplication
  val NHigh = N.splitAt(k - 1)._1 // k+1 bits, high
  val E = msbBcm(Seq(NHigh)).head // k+1 bits
  // third multiplication
  val ME = lsbBcm(Seq(E)).head // k+2 bits
  // subtraction
  val NLow = N.splitAt(k + 2)._2 // k+2 bits, low
  val T = sub(Seq(NLow, ME)).head
  // fine reduction
  val R = rec(Seq(T)).head

  val z = graph.addOutput("Z", k) //
  z := R

  graph

  graphDone()
}
