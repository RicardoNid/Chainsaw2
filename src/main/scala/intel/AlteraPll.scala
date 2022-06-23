package org.datenlord
package intel

import spinal.core.{BlackBox, Bool, in, out}

/** Altera Pll IP blackbox
 *
 * @param outClockCount
 */
case class AlteraPll(outClockCount: Int) extends BlackBox {
  val refclk = in Bool()
  val rst = in Bool()
  val outclk = out Vec(Bool(), outClockCount)
}
