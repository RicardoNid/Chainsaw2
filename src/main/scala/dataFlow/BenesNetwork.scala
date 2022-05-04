package org.datenlord
package dataFlow

import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class BenesNetworkConfig(n: Int, bitWidth: Int, permutations: Seq[Seq[Int]])
  extends TransformConfig {

  override def latency = 0

  override def inputFlow = CyclicFlow(n, permutations.length)

  override def outputFlow = CyclicFlow(n, permutations.length)
}

case class BenesNetwork(n: Int, width: Int) extends Component {

  require(isPow2(n))

  val elemType = HardType(Bits(width bits))
  val controlType = HardType(Bits(n / 2 bits))

  val dataIn = in Vec(elemType, n)
  val dataOut = out Vec(elemType, n)
  val controlIn = in Vec(controlType, 2 * log2Up(n) - 1)

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

    def getUp[T](dataIn: Seq[T]) = dataIn.take(dataIn.size / 2)

    def getBottom[T](dataIn: Seq[T]) = dataIn.takeRight(dataIn.size / 2)

    if (n == 2) switch22(dataIn(0), dataIn(1), controlIn.head(0))
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
  }

  dataOut := Vec(doBenes(dataIn, controlIn.map(_.asBools)))
}

case class BenesNetworkForPermutation(config: BenesNetworkConfig) extends TransformModule[Bits, Bits] {

  import config._

  val period = permutations.length

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidth bits), n))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidth bits), n))

  val core = BenesNetwork(n, bitWidth)
  val controlROM = Mem(permutations.map(Benes.getControlForHard))

  val counter = CounterFreeRun(period)
  when(dataIn.last)(counter.clear())

  core.dataIn := dataIn.fragment
  core.controlIn := controlROM.readAsync(counter.value)
  dataOut.fragment := core.dataOut

  autoValid()
  autoLast()
}

object BenesNetwork {
  def main(args: Array[String]): Unit = {


    SpinalConfig().generateSystemVerilog(BenesNetwork(8, 4))
    SimConfig.withFstWave.compile(BenesNetwork(8, 3)).doSim { dut =>
      //      dut.clockDomain.forkStimulus(2)
      //      dut.clockDomain.waitSampling()
      val perm = Array(0, 1, 3, 2, 5, 7, 6, 4)
      dut.dataIn.zipWithIndex.foreach { case (bits, i) => bits #= i }
      val control = Benes.getControlForSim(perm)
      dut.controlIn.zip(control).foreach { case (bits, int) => bits #= int }
      //      dut.clockDomain.waitSampling(10)
      sleep(10)
      println(s"dut.dataOut.map(_.toInt) = ${dut.dataOut.map(_.toInt)}")
    }

  }
}
