package org.datenlord
package flopoco

import spinal.core._

abstract class FlopocoBlackBox() extends BlackBox {

  val clk = in Bool()
  mapCurrentClockDomain(clk)

  val config: Flopoco

  def asNode: Seq[Bits] => Seq[Bits]
}

