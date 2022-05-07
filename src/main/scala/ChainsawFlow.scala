package org.datenlord

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object ChainsawFlow {

  def apply[T <: Data](fragment: Vec[T], valid: Bool, last: Bool) = {
    val ret = Flow(Fragment(fragment))
    ret.fragment := fragment
    ret.valid := valid
    ret.last := last
    ret
  }

}
