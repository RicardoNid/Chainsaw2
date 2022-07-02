package org.datenlord
package device

import spinal.core._

object Dsp48 {
  // when pre-addition is on, coeffMax - 1
  // when using unsigned operands, all widths - 1
  private def widthCheck(a: Int, b: Int, c: Int, d: Int, isUnsigned: Boolean, preOn: Boolean): Unit = {
    var dataMax = 27
    var coeffMax = 18
    var aluMax = 47
    if (isUnsigned) {
      dataMax -= 1
      coeffMax -= 1
      aluMax -= 1
    }
    if (preOn) coeffMax -= 1
    assert(a <= coeffMax && d <= coeffMax && b <= dataMax && c <= aluMax)
  }

  /** (a ± d) * b ± c, latency = 3, using adreg + mreg + preg */
  def adbc(a: UInt, b: UInt, c: UInt, d: UInt, preMinus: Boolean = false, postMinus: Boolean = false) = {
    widthCheck(a.getBitsWidth, b.getBitsWidth, c.getBitsWidth, d.getBitsWidth, isUnsigned = true, preOn = true)
    val preAdd = if (preMinus) a -^ d else a +^ d
    if (!postMinus) ((preAdd.d(1) * b.d(1)).d(1) +^ c.d(2)).d(1)
    else ((preAdd.d(1) * b.d(1)).d(1) -^ c.d(2)).d(1)
  }

  /** a * b ± c, latency = 2, using mreg + preg*/
  def abc(a: UInt, b: UInt, c: UInt, postMinus: Boolean = false) = {
    widthCheck(a.getBitsWidth, b.getBitsWidth, c.getBitsWidth, 0, isUnsigned = true, preOn = false)
    if (!postMinus) ((a * b).d(1) +^ c.d(1)).d(1)
    else ((a * b).d(1) -^ c.d(1)).d(1)
  }

  /** a * b, latency = 2, using mreg + preg */
  def ab(a: UInt, b: UInt) = {
    widthCheck(a.getBitsWidth, b.getBitsWidth, 0, 0, isUnsigned = true, preOn = false)
    (a * b).d(2)
  }
}