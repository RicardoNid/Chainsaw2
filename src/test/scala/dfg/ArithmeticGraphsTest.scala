package org.datenlord
package dfg

import arithmetic.MultplierMode._

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ArithmeticGraphsTest extends AnyFlatSpec {

  val genCount = 1
  val testCaseCount = 10000
  val complexTestCaseCount = 10000
  val testWidth = 377
  Random.setSeed(42)
  val data = (0 until testCaseCount * 2).map(_ => Random.nextBigInt(testWidth))

  // get a brand new graph every time we need it
  def graphAdd = ArithmeticGraphs.addGraph(testWidth)

  def graphSub = ArithmeticGraphs.subGraph(testWidth)

  def graphFull = ArithmeticGraphs.karatsubaGraph(testWidth, Full)

  def graphLow = ArithmeticGraphs.karatsubaGraph(testWidth, Low)

  def graphSquare = ArithmeticGraphs.karatsubaGraph(testWidth, Square)

  def graphMontMult = ArithmeticGraphs.montgomeryGraph(testWidth, zprizeModulus, square = false, byLUT = false)

  def graphMontSquare = ArithmeticGraphs.montgomeryGraph(testWidth, zprizeModulus, square = true, false)

  val addGolden = (data: Seq[BigInt]) => Seq(data.sum)
  val subGolden = (data: Seq[BigInt]) => Seq(data(0) - data(1))
  val fullMultGolden = (data: Seq[BigInt]) => Seq(data.product)
  val lowMultGolden = (data: Seq[BigInt]) => Seq(data.product % (BigInt(1) << testWidth))
  val squareMultGolden = (data: Seq[BigInt]) => Seq(data.head * data.head)

  val zprizeModulus = algos.ZPrizeMSM.baseModulus
  val R = BigInt(1) << zprizeModulus.bitLength
  val RInverse = R.modInverse(zprizeModulus)
  val NPrime = ((R * RInverse - 1) / zprizeModulus) % zprizeModulus
  val montData = (0 until complexTestCaseCount * 2).map(_ => Random.nextBigInt(testWidth) % zprizeModulus)
  val montTestData = montData.grouped(2).toSeq.flatMap(slice => slice ++ Seq(zprizeModulus, NPrime))

  val montMultGolden = (data: Seq[BigInt]) => {
    val Seq(x, y, modulus, nprime) = data
    val ret = (x * y * RInverse) % modulus
    ret
  }

  val subMetric = (yours: Seq[BigInt], golden: Seq[BigInt]) => yours.zip(golden).forall { case (x, y) =>
    if (y < 0) x - y == (BigInt(1) << testWidth) else x - (BigInt(1) << testWidth) == y
  }
  val montMetric = (yours: Seq[BigInt], golden: Seq[BigInt]) => yours.zip(golden).forall { case (x, y) => x % zprizeModulus == y % zprizeModulus }

  "addGraph" should "work" in (0 until genCount).foreach(_ => TransformTest.test(graphAdd.toTransform(golden = addGolden), data))
  "subGraph" should "work" in (0 until genCount).foreach(_ => TransformTest.test(graphSub.toTransform(golden = subGolden), data, subMetric))

  it should "work for full multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphFull.toTransform(golden = fullMultGolden), data))
  it should "work for low-bit multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphLow.toTransform(golden = lowMultGolden), data))
  it should "work for square multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphSquare.toTransform(golden = squareMultGolden), data.take(testCaseCount / 2).flatMap(d => Seq(d, d))))

  "montgomeryGraph" should "work for modular multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphMontMult.toTransform(), montTestData, montMetric))
  it should "work for modular square multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphMontSquare.toTransform(), montTestData.take(testCaseCount / 2).flatMap(d => Seq(d, d)), montMetric))
}
