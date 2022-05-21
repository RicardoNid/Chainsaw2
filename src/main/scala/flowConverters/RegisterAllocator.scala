package org.datenlord
package flowConverters

object RegisterAllocator {
  def apply(conversion: FlowConversion) = {
    val timeRange = conversion.latency + conversion.period
    val period = conversion.period
    val allocation = RegisterAllocation(timeRange, period)

    def allocForward(initTime: Int, initReg: Int, data: Int) =
      (0 until conversion.lifeCycles(data))
        .foreach(i => allocation.set(initTime + i, initReg + i, data))

    (0 until period).foreach { time =>
      val dataIns = (0 until conversion.rawDataCount)
        .filter(conversion.tIns(_) == time)
        .sortBy(conversion.lifeCycles(_))
      val regs = (0 until allocation.occupation(time).length + dataIns.length)
        .filter(allocation.get(time + 1, _) == -1) // available regs
        .take(dataIns.length) // first w_{in} available regs
      dataIns.zip(regs).foreach { case (data, reg) => allocForward(time + 1, reg, data) }
    }

    allocation
  }
}
