package org.datenlord
package device

import spinal.core._

import scala.language.postfixOps

/** Primitives in unisim library, usePrimitives should be set as true for synth, false for sim
 *@see [[https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/Introduction]] for primitive definitions
 */
class Unisim extends BlackBox {

  if (!usePrimitives) addRTLPath(s"$unisimPath/$definitionName.v")

}

case class CARRY8() extends Unisim {

  val generic: Generic = new Generic {
    val CARRY_TYPE = "SINGLE_CY8"
  }

  val CO, O = out UInt (8 bits)
  val CI, CI_TOP = in UInt (1 bits)
  val DI, S = in UInt (8 bits)

}

case class LUT6_2(init: BigInt) extends Unisim {

  val generic: Generic = new Generic {
    val INIT = B(init, 64 bits)
  }

  val I0, I1, I2, I3, I4, I5 = in Bits (1 bits)
  val O5, O6 = out Bits (1 bits)

}

