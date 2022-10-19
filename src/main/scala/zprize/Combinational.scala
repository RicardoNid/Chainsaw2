package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

abstract class Combinational extends ChainsawGenerator {

  override var latency = 0

  override def implH = null

  def comb(dataIn:Seq[Bits]):Seq[Bits]
}
