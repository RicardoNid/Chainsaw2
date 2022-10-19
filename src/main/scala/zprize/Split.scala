package org.datenlord
package zprize

case class Split(width: Int, lowWidth: Int) extends ChainsawGenerator {

  override def name = s"split_${width}_${lowWidth}"

  override val impl = (dataIn: Seq[Any]) => {
    val (high, low) = dataIn.asInstanceOf[Seq[BigInt]].head.split(lowWidth)
    Seq(high, low)
  }

  override var inputTypes = Seq(UIntInfo(width))
  override var outputTypes = Seq(UIntInfo(width - lowWidth), UIntInfo(lowWidth))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = 0

  override def implH = new ChainsawModule(this) {
    val (a, b) = dataIn.head.splitAt(lowWidth)
    dataOut := Seq(a, b)
  }
}
