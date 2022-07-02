package org.datenlord
package examples

import spinal.core._

import scala.language.postfixOps

case class RetimingTest(retime: Boolean) extends Component {
  val dataIns = in Vec(UInt(17 bits), 4)
  val Seq(a, b, c, d) = dataIns.map(_.d(1))
  val dataOut = out UInt (35 bits)
  // leave pipelining problem to the synthesizer
  if (retime) dataOut := (((a +^ d) * b) + c).d(3)
  // manually designed pipeline
  else dataOut := (((a +^ d).d(1) * b.d(1)).d(1) + c.d(2)).d(1)
}

object RetimingTest0 {
  def main(args: Array[String]): Unit = {
    VivadoImpl(RetimingTest(true), name = "retime")
  }
}

object RetimingTest1 {
  def main(args: Array[String]): Unit = {
    VivadoImpl(RetimingTest(false), name = "noretime")
  }
}
