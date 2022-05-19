package org.datenlord

/** Transform with no implementations, for dataflow analysis / simulation
 *
 */
object TransformConfigForTest {
  def apply(theSize: (Int, Int), theLatency: Int, repetition: Repetition = Repetition.unit, reuse: Reuse = Reuse.unit) =
    new TransformBase {
      override def impl(dataIn: Seq[Any]) = null

      override val size = theSize

      override def latency = theLatency

      override def flowFormat = PeriodicFlow(this, repetition, reuse)

      override def implH = null

    }
}
