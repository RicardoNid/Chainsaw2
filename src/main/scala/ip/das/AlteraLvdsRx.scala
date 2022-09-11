package org.datenlord
package ip.das

import spinal.core._

import scala.language.postfixOps

case class AlteraLvdsRx(inWidth: Int, desFactor: Int) extends BlackBox {
  val rx_inclock, rx_enable = in Bool()
  val rx_in = in Bits (inWidth bits)
  val rx_out = out Bits (inWidth * desFactor bits)
}
