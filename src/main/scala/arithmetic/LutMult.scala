package org.datenlord
package arithmetic

import spinal.core.Component
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


case class LutMult() extends Component {
  val x, y = in UInt (3 bits)
  val r = out(x * y)
}

case class LutConstMult() extends Component {
  val x = in UInt (6 bits)
  val r = out(x * U(63, 6 bits))
}

object LutMult {
  def main(args: Array[String]): Unit = {
    VivadoSynth(LutMult())
    //    VivadoSynth(LutConstMult())
  }
}
