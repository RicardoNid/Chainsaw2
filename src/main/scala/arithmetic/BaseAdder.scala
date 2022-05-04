package org.datenlord
package arithmetic

import spinal.core._

case class BaseAdder(width: Int) extends Component {

  val a, b = in UInt (width bits)
  val sum = out UInt (width bits)
  val carry = out Bool()

  val ret = (a.d(1) +^ b.d(1)).d(1)
  sum := ret.takeLow(width).asUInt
  carry := ret.msb

}

object BaseAdder {
  def main(args: Array[String]): Unit = {
    VivadoImpl(BaseAdder(34), "adder34")
    VivadoImpl(BaseAdder(129), "adder129")
  }
}
