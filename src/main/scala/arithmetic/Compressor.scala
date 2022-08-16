package org.datenlord
package arithmetic

/** define necessary properties for a basic compressor which can be used to build a compressor tree
 *
 */
abstract class Compressor[T] {

  val isFixed: Boolean // if the size is fixed, it is a GPC, otherwise, it is a row compressor
  val widthLimit: Int // for delay consideration

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

  /** hardware implementation
   */
  def impl(bitsInt: BitHeap[T], width: Int): BitHeap[T]

  /** --------
   * utils
   * -------- */

  def inputBitsCount(width: Int) = outputFormat(width).sum

  def outputBitsCount(width: Int) = outputFormat(width).sum

  def reduction(width: Int): Int = inputBitsCount(width) - outputBitsCount(width)

  def efficiency(width: Int): Double = reduction(width).toDouble / cost(width)

  /** this is very beautiful, try it!
   */
  def toString(width: Int) = {
    val dotsIn = BitHeap.fromHeights(inputFormat(width)).toString
    val dotsOut = BitHeap.fromHeights(outputFormat(width)).toString
    val length = outputFormat(width).length
    val arrowLine = s"${" " * (length / 2) * 2}\u2193"
    val shiftedDotsIn = dotsIn.split("\n").map(_.padToLeft(length * 2 - 1, ' ')).mkString("\n")
    s"$shiftedDotsIn\n$arrowLine\n$dotsOut"
  }
}
