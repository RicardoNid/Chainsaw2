package org.datenlord
package comm

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ViterbiTest extends AnyFlatSpec {

  Random.setSeed(42)
  val blockLength = 128

  val constraintLength = 7
  val paddingLength = constraintLength - 1
  val codeGen = Array(171, 133)
  val trellis = Trellis(constraintLength, codeGen)
  val disWidth = 6

  val frameCount = 10
  def getFrame: Array[Int] = Array.fill(blockLength - paddingLength)(Random.nextInt(2)) ++ Array.fill(paddingLength)(0)
  val data = Array.fill(frameCount)(getFrame).flatten // original data, as golden

  val coded = { // coded data, as input
    matlabEngine.putVariable("x", data)
    matlabEngine.eval(s"trellis = poly2trellis($constraintLength, [${codeGen.mkString(", ")}]);")
    matlabEngine.eval("y = convenc(x, trellis);")
    matlabEngine.getVariable[Array[Int]]("y").map(BigInt(_))
      .grouped(trellis.outputBitWidth).toArray
      .map(_.reduce((a, b) => a.##(b, 1)))
  }

  behavior of "trellis"

  it should "work with matlab" in println(s"previous state of 15 can be ${trellis.lookBackMap(15).mkString("\n")}")

  it should "work with minplus" in println(trellis.toMinplus.head)

  behavior of "viterbi algo"

  it should "work on trace back version" in {
    val ret = ViterbiAlgo.viterbiTraceback(coded.map(_.toInt), trellis, 0)
    assert(data.mkString("") == ret.mkString(""))
  }

  behavior of "viterbi hardware"

  it should "work on forwarding part" in ChainsawTest.test(
    ViterbiForwarding(trellis, disWidth, blockLength = blockLength),
    data = coded)

  val discrepancies: Seq[BigInt] = ViterbiAlgo.viterbiForwarding(coded.map(_.toInt), trellis, max = 1 << (6 - 1)).flatten.map(BigInt(_))

  it should "work on backwarding part" in ChainsawTest.test(
    ViterbiBackwarding(trellis, disWidth, blockLength = blockLength),
    data = discrepancies
  )

  def viterbiMetric: Metric = (yours: Seq[Any], golden: Seq[Any]) => yours.reverse.equals(golden)

  it should "work" in ChainsawTest.test(
    Viterbi(trellis, blockLength, disWidth, copies = 1),
    data = coded,
    golden = data.map(BigInt(_)),
    metric = viterbiMetric)

  it should "impl at disWidth = 6" in ChainsawSynth(Viterbi(trellis, blockLength, 6, copies = 1))
  it should "impl at disWidth = 4" in ChainsawSynth(Viterbi(trellis, blockLength, 4, copies = 1))

}
