package org.datenlord
package flowConverters

import spinal.core._
import spinal.lib._

// TODO: implement this by DFG
case class BenesNetworkCore(N: Int, bitWidth: Int, stagesPerCycle: Int) extends Component {

  require(isPow2(N))

  val elemType = HardType(Bits(bitWidth bits))
  val controlType = HardType(Bits(N / 2 bits))

  val dataIn = in Vec(elemType, N)
  val dataOut = out Vec(elemType, N)
  val controlIn = in Vec(controlType, 2 * log2Up(N) - 1)

  def switch22(a: Bits, b: Bits, switch: Bool) = {
    val (retA, retB) = (cloneOf(a), cloneOf(b))
    when(switch) {
      retA := b
      retB := a
    }.otherwise {
      retA := a
      retB := b
    }
    Vec(retA, retB)
  }

  def doBenes(dataIn: Seq[Bits], controlIn: Seq[Seq[Bool]]): Seq[Bits] = {

    val n = dataIn.size
    val currentStage = log2Up(n) - 1 // stage number, start from the last stage

    def getUp[T](dataIn: Seq[T]) = dataIn.take(dataIn.size / 2)

    def getBottom[T](dataIn: Seq[T]) = dataIn.takeRight(dataIn.size / 2)

    val ret = if (n == 2) switch22(dataIn(0), dataIn(1), controlIn.head(0))
    else {
      // decompose controlIn
      val (pre, post, mid) = (controlIn.head, controlIn.last, controlIn.drop(1).dropRight(1))
      val (subNetworkUp, subNetworkBottom) = (mid.map(getUp), mid.map(getBottom))
      // build network
      val afterPre = getUp(dataIn).zip(getBottom(dataIn)).zip(pre)
        .map { case ((a, b), switch) => switch22(a, b, switch) } // switches
      val afterPreOrdered = afterPre.map(_.head) ++ afterPre.map(_.last) // connections
      val afterSub = doBenes(getUp(afterPreOrdered), subNetworkUp) ++ doBenes(getBottom(afterPreOrdered), subNetworkBottom) // connections
      val afterPost = getUp(afterSub).zip(getBottom(afterSub)).zip(post)
        .map { case ((a, b), switch) => switch22(a, b, switch) } // switches
      afterPost.map(_.head) ++ afterPost.map(_.last) // connections
    }
    ret
  }

  dataOut := Vec(doBenes(dataIn, controlIn.map(_.asBools)))
}

// TODO: implement the function of stagesPerCycle(by DFG?)

/** configuration for periodic Benes network
 *
 * @param N              size of input/output vector
 * @param bitWidth       bit width of elements in input/output vector
 * @param permutations   a group of permutations executed by the network periodically
 * @param stagesPerCycle number of cascaded switches in between pipeline registers
 * @see ''MATHEMATICS FOR COMPUTER SCIENCE'' Chapter 11
 */
case class BenesNetworkConfig(N: Int, bitWidth: Int, permutations: Seq[Seq[Int]], stagesPerCycle: Int)
  extends TransformBase {

  val p = permutations.length

  override val size = (N * p, N * p)

  override def latency = 0

  override val spaceFold = p

  override def impl(dataIn: Seq[Any]) =
    dataIn.asInstanceOf[Seq[BigInt]].grouped(N).toSeq.zip(permutations)
      .flatMap { case (data, perm) => perm.map(index => data(index)) }

  override def implH = BenesNetwork(this)
}

/** periodic Benes network
 */
case class BenesNetwork(config: BenesNetworkConfig) extends TransformModule[Bits, Bits] {

  import config._

  val period = permutations.length

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidth bits), N))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidth bits), N))

  val core = BenesNetworkCore(N, bitWidth, stagesPerCycle)
  val controlROM = Mem(permutations.map(Benes.getControlForHard))

  val counter = CounterFreeRun(period)
  when(dataIn.last)(counter.clear())

  core.dataIn := dataIn.fragment
  core.controlIn := controlROM.readAsync(counter.value)
  dataOut.fragment := core.dataOut

  autoValid()
  autoLast()
}