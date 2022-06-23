package org.datenlord
package intel

import spinal.core._

case class AlteraFIFO(width: Int) extends BlackBox {
  val rdclk, wrclk = in Bool()
  val data = in Bits (width bits)
  val q = out Bits (width bits)
  val rdreq, wrreq = in Bool()
  val rdempty, wrfull = out Bool()
}
