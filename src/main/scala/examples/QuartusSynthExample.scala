package org.datenlord
package examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class QuartusSynthExample() extends Component{

  val a,b = in UInt(128 bits)
  val c = out UInt(129 bits)

  c := (a.d(1) +^ b.d(1)).d(1)
}

object QuartusSynthExample {
  def main(args: Array[String]): Unit = {

    import intel.QuartusFlow
    new QuartusFlow(QuartusSynthExample()).impl()

  }
}
