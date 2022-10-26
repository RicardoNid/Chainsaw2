package org.datenlord
package examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class AddSub() extends Component {

  val a = in UInt(4 bits)
  val c = out UInt(4 bits)

  val his = History(a, 4)

  c := his(3)

}

object AddSub extends App {
  VivadoSynth(AddSub())
}
