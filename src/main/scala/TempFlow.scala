package org.datenlord

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

// TODO: using Flow[Fragment[..]] will lead to combinational loop, why?
case class TempFlow(fragment: Vec[Bits], valid: Bool, last: Bool)
