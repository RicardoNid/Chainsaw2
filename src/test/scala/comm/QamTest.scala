package org.datenlord
package comm

import breeze.numerics.sqrt
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class QamTest extends AnyFlatSpec {

  val parallel = 256
  val testCount = parallel * 10
  val bitsAllocated = 4
  val M = 1 << bitsAllocated
  val numericType = ComplexFixInfo(2, 16)
  val powerFactor = 0.5

  val bitAlloc = Seq.fill(parallel)(bitsAllocated)
  //  val bitAlloc = Seq(2, 4, 6, 8)
  val powAlloc = Seq.fill(parallel)(powerFactor)

  val data = Array.fill(testCount)(Random.nextInt(M))
  val mapped = QamAlgo.qammod(data, M).map(_ * sqrt(powerFactor))

  val bits = Array.fill(testCount * bitsAllocated)(BigInt(1, Random))

  behavior of "qam algorithms"

  it should "work on map-demap mode" in { // TODO: test the algorithms by Matlab
    val mapped = QamAlgo.qammod(data, M)
    val recovered = QamAlgo.qamdemod(mapped, M)
    assert(recovered.sameElements(data))
  }

  behavior of "qamdemod"

  it should "work" in ChainsawTest.test( // FIXME: this failed when bitAlloc > 4, please find the reason
    QamdemodWithAlloc(bitAlloc, powAlloc, numericType),
    data = mapped
  )

  behavior of "qammod"

  it should "work" in ChainsawTest.test(
    QammodWithAlloc(bitAlloc, powAlloc, numericType),
    data = bits,
    metric = ChainsawMetric.complexAbs(1e-2)
  )

}
