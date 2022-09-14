package org.datenlord
package arithmetic

import spinal.core._

/** describe the attributes of an UInt Operand needed for bit heap compression
 *
 * @param width  width of the operand
 * @param weight weight of the operand
 * @param sign   signedness of the operand
 * @param time   delay of the operand
 */
case class ArithInfo(width: Int, weight: Int, sign: Boolean = true, time: Int = 0) {
  val low = weight
  val high = low + width
  val range = (high - 1) downto low

  def <<(shiftLeft: Int) = ArithInfo(width, weight + shiftLeft, sign)

  def unary_- = ArithInfo(width, weight, !sign)
}
