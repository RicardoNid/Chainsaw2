package org.datenlord
package ip.das

import spinal.core._

import scala.language.postfixOps
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class DcFifo(width: Int) extends BlackBox {
  val rdclk, wrclk = in Bool()
  val data = in Bits (width bits)
  val q = out Bits (width bits)
  val rdreq, wrreq = in Bool()
  val rdempty, wrfull = out Bool()
}

case class FifoReadInterface(width: Int) extends Bundle with IMasterSlave {
  val rdreq, rdempty = Bool()
  val q = Bits(width bits)

  override def asMaster(): Unit = {
    in(rdempty, q)
    out(rdreq)
  }
}

case class FifoWriteInterface(width: Int) extends Bundle with IMasterSlave {
  val wrreq, wrfull = Bool()
  val data = Bits(width bits)

  override def asMaster(): Unit = {
    in(wrfull)
    out(wrreq, data)
  }
}