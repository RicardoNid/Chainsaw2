package org.datenlord
package zprize

import ip.ftn.Dft
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class DftTest extends AnyFlatSpec {

  val data = Seq.fill(16)(Random.nextComplex())
  val invList = Seq(false, true)
  val sizeList = Seq(2,4,8)
  val dataType = ComplexFixInfo(4, 13)

  "dft" should "work for all configurations" in Seq.tabulate(2,3)((i, j) =>
    ChainsawTest.test(Dft(sizeList(j), invList(i), dataType, 16),
      data,
      metric = ChainsawMetric.complexAbs(1e-1)))

  it should "impl for all configurations" in Seq.tabulate(2,3)((i, j) =>
    VivadoImpl(Dft(sizeList(j), invList(i), dataType, 16).implH))

}
