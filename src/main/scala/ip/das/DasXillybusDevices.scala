package org.datenlord
package ip.das

/** device file configurations for our DAS system
 *
 */
object DasXillybusDevices {
  def apply(): Seq[XillybusDevice] = {
    val info0 = XillybusDevice("info_0_8", "fifo", "read", 8) // 1MB/s
    val info1 = XillybusDevice("info_1_8", "fifo", "read", 8) // 1MB/s
    val adc0 = XillybusDevice("adc_0_16", "fifo", "read", 16) // 100MB/s
    val adc1 = XillybusDevice("adc_1_16", "fifo", "read", 16) // 100MB/s
    val ctrl = XillybusDevice("ctrl_8", "mem", "write", 8, 5) // 0.1MB/s
    val write = XillybusDevice("write_16", "fifo", "write", 16) // 10MB/s
    Seq(adc0, adc1, info0, info1, ctrl, write)
  }

  def ctrl = XillybusDevice("ctrl_8", "mem", "write", 8, 5) // 0.1MB/s

  // TODO: this should be a xillybus wrapper with specific fields of channel interfaces
}
