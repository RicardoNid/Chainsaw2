package ip.pippenger

import org.datenlord.logger

import scala.collection.mutable

object PippengerNew {

  // simulation of Pippenger hardware

  def doPipenger1(scalars: Array[BigInt], points: Array[BigInt], N: Int, W: Int, w: Int, latency: Int): (Array[Array[Array[BigInt]]], BigInt) = {

    require(scalars.length == N && points.length == N)
    require(scalars.forall(_.bitLength <= W))

    case class AdderData(
                          data: BigInt,
                          G: Int, B: Int, C: Int,
                          valid: Boolean,
                          end: Boolean = false) //for debug

    def bubble: AdderData = AdderData(0, 0, 0, 0, false)

    def end: AdderData = AdderData(0, 0, 0, 0, false, true)

    val GMax = math.ceil(W.toDouble / w).toInt
    val BMax = (1 << w) - 1
    val CMax = math.ceil(latency.toDouble / GMax).toInt // copy count

    logger.info(s"storage need: ${CMax * GMax * (BMax + 1) * W / 8.toDouble / 1e6} MB")

    var time = BigInt(0)
    val adder = mutable.Queue.fill[AdderData](latency)(bubble)
    val mem = Array.fill(GMax)(Array.fill(BMax)(Array.fill(CMax)(BigInt(0)))) //

    var cNow = 0

    for (i <- 0 until N) {
      for (j <- 0 until GMax) {
        val bNow = ((scalars(i) >> (w * j)) & ((1 << w) - 1)).toInt - 1
        if (bNow >= 0) {
          adder.enqueue(AdderData(points(i) + mem(j)(bNow)(cNow), j, bNow, cNow, true))
        } else {
          adder.enqueue(bubble) // this can be avoid
        }
        val adderOutput = adder.dequeue()
        if (adderOutput.valid) {
          mem(adderOutput.G)(adderOutput.B)(adderOutput.C) = adderOutput.data
        }
        time += 1
      }
      cNow += 1
      if (cNow >= CMax) {
        cNow = 0
      }
    }
    adder.enqueue(end)

    var adderOutput = adder.dequeue()

    while (!adderOutput.end) {
      if (adderOutput.valid) {
        mem(adderOutput.G)(adderOutput.B)(adderOutput.C) = adderOutput.data
      }
      time += 1
      adderOutput = adder.dequeue()
    }

    (mem, time)
  }

  def main(args: Array[String]): Unit = {
    val N = 1024 * 1024
    val W = 253
    val w = 4
    val latency = 300

    val scalars = Array.fill(N)(BigInt(W, scala.util.Random))
    val points = Array.fill(N)(BigInt(4, scala.util.Random))
    val golden = scalars.zip(points).map { case (q, p) => p * q }.sum

    val (mem, time) = doPipenger1(scalars, points, N, W, w, latency)

    print(s"time is ${time}.\n")

    //check

    val mem2 = mem.map(_.map(_.sum)) // sum every mem
    val mem3 = mem2.map(_.zipWithIndex.map { case (m, i) => m * (i + 1) }) //mul
    val output = mem3.zipWithIndex.map { case (m, i) => m.sum << (i * w) }.sum

    if (output == golden) {
      print("Output no problem.\n")
    } else {
      print("Output error.\n")
    }
  }
}