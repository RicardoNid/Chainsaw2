package org.datenlord
package flowConverters

import spinal.core._
import spinal.lib._

object ForwardRegisterAllocator {
  def apply(conversion: FlowConversion) = {
    val lifeCycleTable = conversion.lifeTimeTable
    val timeRange = lifeCycleTable.latency + conversion.period
    val period = conversion.period
    val allocation = RegisterAllocation(timeRange + 1, period)
    println(lifeCycleTable)
    println(s"range: $timeRange")
    println(s"period: $period")

    def allocForward(initTime: Int, initReg: Int, data: Int): Unit =
      (0 until lifeCycleTable.lifeLengths(data))
        .foreach(i => allocation.set(initTime + i, initReg + i, data))

    (0 until period).foreach { time =>
      println(time)
      val dataIns = (0 until conversion.rawDataCount)
        .filter(conversion.tIns(_) == time)
        .sortBy(lifeCycleTable.lifeLengths(_))
      val regs = (0 until allocation.occupation(time).length + dataIns.length)
        .filter(allocation.get(time + 1, _) == -1) // available regs
        .take(dataIns.length) // first w_{in} available regs
      println(allocation)
      dataIns.zip(regs).foreach { case (data, reg) => allocForward(time + 1, reg, data) }
    }

    allocation
  }
}

case class ForwardRegisterConverterConfig[T <: Data](conversion: FlowConversion, dataType: HardType[T])
  extends TransformConfig {

  override def impl(dataIn: Seq[Any]) = dataIn

  override val size = (1, 1) // don't care

  override def latency = conversion.lifeTimeTable.latency

  override def flowFormat = MeshFormat.dontCare

  override def inputFlow = conversion.flowInPadded

  override def outputFlow = conversion.flowOutPadded

  override def implH = ForwardRegisterConverter(this)
}

case class ForwardRegisterConverter[T <: Data]
(config: ForwardRegisterConverterConfig[T])
  extends TransformModule[T, T] {

  import config._

  val registerAllocation = ForwardRegisterAllocator(conversion)

  val dataIn = slave Flow Fragment(Vec(dataType(), conversion.flowIn.portWidth))
  val dataOut = master Flow Fragment(Vec(dataType(), conversion.flowOut.portWidth))

  val counter = CounterFreeRun(conversion.period)
  when(dataIn.last)(counter.clear())

  // basic connection
  val registers = Seq.fill(registerAllocation.registerCount)(Reg(dataType()))
  registers.head.assignDontCare()
  registers.sliding(2).foreach { pair => pair(1) := pair(0) }
  dataOut.payload.foreach(_.assignDontCare())

  def getTimeIn(data: Int) = conversion.flowIn.getTime(data)

  def getTimeOut(data: Int) = conversion.flowOut.getTime(data) + latency

  def getPortIn(data: Int) = conversion.flowIn.getPort(data)

  def getPortOut(data: Int) = conversion.flowOut.getPort(data)

  // input connections
  (0 until conversion.rawDataCount).foreach { data =>
    val time = getTimeIn(data)
    val registerIndex = registerAllocation.occupation(time + 1).indexOf(data)
    if (registerIndex == -1) when(counter.value === time)(dataOut.payload(getPortOut(data)) := dataIn.payload(getPortIn(data)))
    else when(counter.value === time)(registers(registerIndex) := dataIn.payload(getPortIn(data)))
  }

  // output connections
  (0 until conversion.rawDataCount).foreach { data =>
    val time = getTimeOut(data)
    val registerIndex = registerAllocation.occupation(time).indexOf(data)
    if (registerIndex != -1) when(counter.value === time)(dataOut.payload(getPortOut(data)) := registers(registerIndex))
  }

  // controls
  switch(counter.value) {
    conversion.flowOut.validCycles.foreach(time => is(time)(dataOut.valid := True))
    if(conversion.flowOut.validCycles.length < conversion.period) default(dataOut.valid := False)
  }
  autoLast()
}
