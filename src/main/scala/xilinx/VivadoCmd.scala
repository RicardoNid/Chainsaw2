package org.datenlord
package xilinx

import scala.sys.process._

object VivadoCmd {

  def exec(command: String) = {
    Process(s"/tools/Xilinx/Vivado/2021.1/bin/vivado -mode tcl")
    Process("pwd")
  }

  def main(args: Array[String]): Unit = {
    VivadoCmd.exec("read_verilog ./top.v")
  }

}
