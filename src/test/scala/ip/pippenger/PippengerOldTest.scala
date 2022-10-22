package org.datenlord
package ip.pippenger

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PippengerOldTest extends AnyFlatSpec {

  "Pippenger" should "make a good estimation on cost" in {

    val W = 253
    val add = (a: BigInt, b: BigInt) => a + b
    val dbl = (a: BigInt) => 2 * a

    Seq.tabulate(1, 1) { (a, b) =>
      val N = (a + 1) * 100000
      val w = b + 12
      val scalars = (0 until N).map(_ => Random.nextBigInt(253))
      val points = (0 until N).map(_ => Random.nextBigInt(8))
      val cost = PippengerOld(N, W, w, add, dbl, BigInt(0)).doPippenger(scalars, points)
      val costEstimated = PippengerOld.estimateWorkload(N, W, w)
      val err1 = (costEstimated._1 - cost._1).abs.toDouble / cost._1
      val err2 = (costEstimated._2 - cost._2).abs.toDouble / cost._2
      val err3 = (costEstimated._3 - cost._3).abs.toDouble / cost._3

      assert(err1 <= 0.1)

      println(s"err      : $err1, stage1: $err2, stage2: $err3")
      println(s"estimated: ${costEstimated._1}, stage1: ${costEstimated._2}, stage2: ${costEstimated._3}")
      println(s"actual   : ${cost._1}, stage1: ${cost._2}, stage2: ${cost._3}")
    }
  }

  it should "show estimation for different w" in {
    val candidates = Seq(4, 10, 12, 17, 18, 19)
    candidates.map(PippengerOld.estimateWorkload(1 << 26, 253, _))
  }

  it should "work for ZPRIZE MSM" in {

  }

}
