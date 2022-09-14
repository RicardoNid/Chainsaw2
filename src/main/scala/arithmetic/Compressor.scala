package org.datenlord
package arithmetic

import xilinx._

/** define necessary properties for a basic compressor which can be used to build a compressor tree
 *
 */
abstract class Compressor[T] {

  val name = getClass.getSimpleName.init

  val isFixed: Boolean // if the size is fixed, it is a GPC, otherwise, it is a row compressor

  val widthMax: Int // for delay consideration

  // TODO: widthMin

  /** --------
   * key definitions
   * -------- */

  /** number of bits in input columns, low to high
   */
  def inputFormat(width: Int): Seq[Int]

  /** number of bits in output columns, low to high
   */
  def outputFormat(width: Int): Seq[Int]

  /** number of LUTs
   */
  def cost(width: Int): Int

  /** hardware implementation, the compressor is responsible for padding zeros
   */
  def impl(bitsIn: BitHeap[T], width: Int): BitHeap[T]

  /** --------
   * utils
   * -------- */

  def inputBitsCount(width: Int) = inputFormat(width).sum

  def outputBitsCount(width: Int) = outputFormat(width).sum

  def reduction(width: Int): Int = inputBitsCount(width) - outputBitsCount(width)

  def efficiency(width: Int): Double = reduction(width).toDouble / cost(width)

  def utilRequirement(width: Int): VivadoUtil

  /** this is very beautiful, try it!
   */
  def toString(width: Int) = {
    val dotsIn = BitHeap.getFakeHeapFromHeights(inputFormat(width)).toString
    val dotsOut = BitHeap.getFakeHeapFromHeights(outputFormat(width)).toString
    val length = outputFormat(width).length
    val arrowLine = s"${" " * (length / 2) * 2}\u2193"
    val shiftedDotsIn = dotsIn.split("\n").map(_.padToLeft(length * 2 - 1, ' ')).mkString("\n")
    s"$shiftedDotsIn\n$arrowLine\n$dotsOut"
  }
}
