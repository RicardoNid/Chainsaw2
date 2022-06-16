package org.datenlord
package ip.das

import spinal.core._

/** Altera Pll IP blackbox
 * @param outClockCount
 */
case class AlteraPll(outClockCount: Int) extends BlackBox {
  val refclk = in Bool()
  val rst = in Bool()
  val outclk = out Vec(Bool(), outClockCount)
}