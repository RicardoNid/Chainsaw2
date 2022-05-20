package org.datenlord

object Example extends App {

  // basic
  val basic = TransformConfigForTest((2, 2), 1)
  val spaceFold = Reuse(1, 1, 2, 1)
  val timeFold = Reuse(1, 1, 1, 2)
  val noReuse = Reuse.unit
  val noRepeat = Repetition.unit

  println("basic")
  println(MeshFlow(basic, noRepeat, noReuse).inputFlow)
  println(MeshFlow(basic, noRepeat, spaceFold).inputFlow)
  println(MeshFlow(basic, noRepeat, timeFold).outputFlow)

  // complex mesh flow behavior
  val config0: TransformBase = TransformConfigForTest((2, 2), 3)
  val config1: TransformBase = TransformConfigForTest((2, 2), 4)
  val config2: TransformBase = TransformConfigForTest((2, 2), 5)

  val repeat = Repetition(Seq(SpaceRepetition(2, 1), SpaceRepetition(2)), TimeRepetition(2))
  val reuse0 = Reuse(2, 2, 2, 1)
  val reuse1 = Reuse(2, 2, 1, 2)
  val reuse2 = Reuse(2, 2, 1, 1)
  // no bubble, fifo
  println(MeshFlow(config0, repeat, reuse0).inputFlow)
  println(MeshFlow(config0, repeat, reuse0).outputFlow)
  // no bubble, no fifo
  println(MeshFlow(config1, repeat, reuse0).inputFlow)
  println(MeshFlow(config1, repeat, reuse0).outputFlow)
  // bubble, fifo
  println(MeshFlow(config2, repeat, reuse0).inputFlow)
  println(MeshFlow(config2, repeat, reuse0).outputFlow)
  // no bubble, fifo
  println(MeshFlow(config0, repeat, reuse1).inputFlow)
  println(MeshFlow(config0, repeat, reuse1).outputFlow)
  // no bubble, no fifo
  println(MeshFlow(config1, repeat, reuse1).inputFlow)
  println(MeshFlow(config1, repeat, reuse1).outputFlow)
  // bubble, fifo
  println(MeshFlow(config2, repeat, reuse1).inputFlow)
  println(MeshFlow(config2, repeat, reuse1).inputFlow)

  // life cycles of registers in dataflow conversions
  println(flowConverters.FlowConversion(MeshFlow(config0, repeat, reuse0).outputFlow, MeshFlow(config0, repeat, reuse1).inputFlow).toKaTex)

}
