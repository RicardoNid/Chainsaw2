package org.datenlord
package device

import spinal.core._

import scala.language.postfixOps

/** Ternary adder/subtractor which can be recognized by Xilinx synthesis tools
 * @param width width of input operands, output width = width + 2
 * @param sub sub = 0 => a+b+c 1 => a+b-c 2 => a-b-c
 */
case class TernaryAdder(width: Int, sub:Int = 0) extends Component {

  val dataIn = in Vec(UInt(width bits), 3)
  val dataOut = out UInt (width + 2 bits)

  val impl = TernaryAdderXilinx(width, sub)
  impl.dataIn := dataIn
  dataOut := impl.dataOut.d(1)

}

object TernaryAdder {
  def main(args: Array[String]): Unit = {
    //    SpinalConfig().generateVerilog(TernaryAdder(7))
    VivadoSynth(TernaryAdder(126, 0)) // A+B+C
    VivadoSynth(TernaryAdder(126, 1)) // A+B-C
    VivadoSynth(TernaryAdder(126, 2)) // A-B-C
  }
}

case class TernaryAdderXilinx(width: Int, sub: Int) extends BlackBox {
  val dataIn = in Vec(UInt(width bits), 3)
  val dataOut = out UInt (width + 2 bits)
  val generic = new Generic {
    val width = TernaryAdderXilinx.this.width
  }

  sub match {
    case 0 => addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/blackboxese/TernaryAdder.v")
    case 1 => addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/blackboxese/TernaryAddSub.v")
    case 2 => addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/blackboxese/TernaryAddSubSub.v")
  }
}
