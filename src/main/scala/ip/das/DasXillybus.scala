package org.datenlord
package ip.das

import spinal.core.SpinalConfig

object DasXillybus {
  def apply() = {

    val info0 = XillybusDevice("info_0_8", "fifo", "read", 8) // 1MB/s
    val info1 = XillybusDevice("info_1_8", "fifo", "read", 8) // 1MB/s
    val adc0 = XillybusDevice("adc_0_16", "fifo", "read", 16) // 100MB/s
    val adc1 = XillybusDevice("adc_1_16", "fifo", "read", 16) // 100MB/s
    val ctrl = XillybusDevice("ctrl_8", "mem", "write", 8, 5) // 0.1MB/s
    val write = XillybusDevice("write_16", "fifo", "write", 16) // 10MB/s
    val devices  = Seq(adc0, adc1, info0, info1, ctrl,  write)

    Xillybus(devices)
  }

  def main(args: Array[String]): Unit = {
    SpinalConfig().generateVerilog(DasXillybus())
  }
}
