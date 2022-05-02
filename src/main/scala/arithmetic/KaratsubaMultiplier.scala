package org.datenlord
package arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class KaratsubaMultiplier(multWidth: Int, wordWidth: Int = 16) extends Component {

  val dataIn = slave Flow UInt(multWidth bits)
  val dataOut = master Flow UInt(multWidth bits)



}
