package org.datenlord

object Example extends App {

  // basic
  val basic = TransformConfigForTest((2, 2), 1)
  val spaceFold = Reuse(1, 1, 2, 1)
  val timeFold = Reuse(1, 1, 1, 2)
  val noReuse = Reuse.unit
  val noRepeat = Repetition.unit

  println("basic")
  println(MeshFormat(basic, noRepeat, noReuse).inputFlow)
  println(MeshFormat(basic, noRepeat, spaceFold).inputFlow)
  println(MeshFormat(basic, noRepeat, timeFold).outputFlow)

  // complex mesh flow behavior
  val config0: TransformBase = TransformConfigForTest((2, 2), 3)
  val config1: TransformBase = TransformConfigForTest((2, 2), 4)
  val config2: TransformBase = TransformConfigForTest((2, 2), 5)

  val repeat = Repetition(Seq(SpaceRepetition(2, 1), SpaceRepetition(2)), TimeRepetition(2))
  val reuse0 = Reuse(2, 2, 2, 1)
  val reuse1 = Reuse(2, 2, 1, 2)
  val reuse2 = Reuse(2, 2, 1, 1)
  // no bubble, fifo
  println(MeshFormat(config0, repeat, reuse0).inputFlow)
  println(MeshFormat(config0, repeat, reuse0).outputFlow)
  // no bubble, no fifo
  println(MeshFormat(config1, repeat, reuse0).inputFlow)
  println(MeshFormat(config1, repeat, reuse0).outputFlow)
  // bubble, fifo
  println(MeshFormat(config2, repeat, reuse0).inputFlow)
  println(MeshFormat(config2, repeat, reuse0).outputFlow)
  // no bubble, fifo
  println(MeshFormat(config0, repeat, reuse1).inputFlow)
  println(MeshFormat(config0, repeat, reuse1).outputFlow)
  // no bubble, no fifo
  println(MeshFormat(config1, repeat, reuse1).inputFlow)
  println(MeshFormat(config1, repeat, reuse1).outputFlow)
  // bubble, fifo
  println(MeshFormat(config2, repeat, reuse1).inputFlow)
  println(MeshFormat(config2, repeat, reuse1).inputFlow)

  // life cycles of registers in dataflow conversions
  val repeat1 = Repetition(Seq(SpaceRepetition(4)), TimeRepetition(2))
  val formatA = MeshFormat(config0, repeat1, reuse0)
  val formatB = MeshFormat(config0, repeat1, reuse1)
  val formatC = MeshFormat(config0, repeat1, reuse2)
  val reuse3 = Reuse(4,1,1,1)
  val formatD = MeshFormat(config0, repeat1, reuse3)
  println(flowConverters.FlowConversion(formatA, formatB).lifeTimeTable)
  println(flowConverters.FlowConversion(formatB, formatC).lifeTimeTable)
  println(flowConverters.FlowConversion(formatC, formatD).lifeTimeTable)

  import flowConverters.{FlowConversion, ForwardRegisterAllocator, RegisterAllocation}
  println(ForwardRegisterAllocator(FlowConversion(formatC, formatD)))
}
