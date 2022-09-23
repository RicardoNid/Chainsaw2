package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** --------
 * interfaces for reading/writing an Altera FIFO, which is controlled by req & empty/full signal
 -------- */
case class FifoWriteInterface(width: Int)
  extends Bundle with IMasterSlave {
  val wrreq, wrfull = Bool()
  val data = Bits(width bits)

  override def asMaster(): Unit = {
    in(wrfull)
    out(wrreq, data)
  }
}

case class FifoReadInterface(width: Int)
  extends Bundle with IMasterSlave {
  val rdreq, rdempty = Bool()
  val q = Bits(width bits)

  override def asMaster(): Unit = {
    in(rdempty, q)
    out(rdreq)
  }
}

/** black box of altera dcfifo("dc" means different clocks are used for input and output)
 */
case class AlteraDcFifo(width: Int) extends BlackBox {
  val rdclk, wrclk = in Bool()
  val data = in Bits (width bits)
  val q = out Bits (width bits)
  val rdreq, wrreq = in Bool()
  val rdempty, wrfull = out Bool()
}

