package org.datenlord
package flowConverters

import spinal.core._
import spinal.lib._

object ConversionMode extends Enumeration {
  val Forward = Value
  type ConversionMode = Value
}

case class FlowConverterConfig[T <: Data](conversion: FlowConversion, hardType: HardType[T])
  extends TransformConfig {

  import ConversionMode._

  override def impl(dataIn: Seq[Any]) = dataIn

  override val size = (1, 1) // don't care

  def getConversionMode: (ConversionMode, Any) = (Forward, RegisterAllocator(conversion))

  override def latency = getConversionMode._1 match {
    case Forward => conversion.latency
  }

  override def flowFormat = MeshFormat.dontCare

  override def inputFlow = conversion.flowInPadded

  override def outputFlow = conversion.flowOutPadded

  override def implH = getConversionMode._1 match {
    case Forward => FlowConverter(this)
  }
}

case class FlowConverter[T <: Data]
(config: FlowConverterConfig[T])
  extends TransformModule[T, T] {

  import config._

  val registerAllocation = RegisterAllocator(conversion)

  val dataIn = slave Flow Fragment(Vec(hardType(), conversion.flowIn.portWidth))
  val dataOut = master Flow Fragment(Vec(hardType(), conversion.flowOut.portWidth))

  val counter = CounterFreeRun(conversion.period)
  when(dataIn.last)(counter.clear())

  // basic connection
  val registers = Seq.fill(registerAllocation.registerCount)(Reg(hardType()))
  registers.head.assignDontCare()
  registers.sliding(2).foreach { pair => pair(1) := pair(0) }
  dataOut.payload.foreach(_.assignDontCare())

  def getTimeIn(data: Int) = conversion.flowIn.getTime(data)

  def getTimeOut(data: Int) = conversion.flowOut.getTime(data) + conversion.latency

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

  // TODO: inner connections(for forward allocation, this is not necessary)

  // controls
  switch(counter.value) {
    conversion.flowOut.validCycles.foreach(time => is(time)(dataOut.valid := True))
    default(dataOut.valid := False)
  }
  dataOut.last := Delay(dataIn.last, conversion.latency, init = False)
}
