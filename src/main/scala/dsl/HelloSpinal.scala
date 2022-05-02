package org.datenlord
package dsl

import spinal.core._

object HelloSpinal {

  class Adder extends Component {
    val a,b = in UInt(4 bits)
    val ret = out(a + b)
  }

  def main(args: Array[String]): Unit = {
    SpinalConfig().generateVerilog(new Adder)
  }

}
