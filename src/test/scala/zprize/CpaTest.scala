package org.datenlord
package zprize

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CpaTest extends AnyFlatSpec {

  "cpa" should "work under 4 different pipeline modes" in {
    val m2s = Cpa(BinaryAdder, Seq.fill(4)(10), M2S)
    val s2m = Cpa(BinaryAdder, Seq.fill(4)(10), S2M)
    val s2s = Cpa(BinaryAdder, Seq.fill(4)(10), S2S)
    val m2m = Cpa(BinaryAdder, Seq.fill(4)(10), M2M)

    val data = Seq.fill(1000)(BigInt(10, Random))
    ChainsawTest.test(s2s, data)
    ChainsawTest.test(s2m, data)
    ChainsawTest.test(m2s, data)
    ChainsawTest.test(m2m, data)
  }


  it should "work under 5 different adder modes" in {

    val widths = Seq.fill(4)(10)

    val add = Cpa(BinaryAdder, Seq.fill(4)(10), M2M)
    val sub = Cpa(BinarySubtractor, Seq.fill(4)(10), M2M)
    val ter0 = Cpa(TernaryAdder, Seq.fill(4)(10), M2M)
    val ter1 = Cpa(TernarySubtractor1, Seq.fill(4)(10), M2M)
    val ter2 = Cpa(TernarySubtractor2, Seq.fill(4)(10), M2M)

    val cpaMetric: Metric = (yours: Seq[Any], golden: Seq[Any]) => {
      val str = yours.asInstanceOf[Seq[BigInt]].zip(widths).map { case (int, i) =>
        int.toString(2).padToLeft(i, '0')
      }.reverse.reduce(_ + _)
      val ret = BigInt(str, 2)
      (golden.asInstanceOf[Seq[BigInt]].head < 0) || golden.head == ret
    }

    // TODO: implementation of ternary adder
    val data = Seq.fill(1200)(BigInt(10, Random))
    Seq(add, sub, ter0, ter1, ter2).foreach(gen => ChainsawTest.test(gen, data, metric = cpaMetric))
  }
}
