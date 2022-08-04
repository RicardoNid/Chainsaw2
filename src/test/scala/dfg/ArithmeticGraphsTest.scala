package org.datenlord
package dfg

import arithmetic.MultplierMode._

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ArithmeticGraphsTest extends AnyFlatSpec {

  val genCount = 1
  val testCaseCount = 10000
  val complexTestCaseCount = 100
  val testWidth = 377
  Random.setSeed(42)
  val data = (0 until testCaseCount * 2).map(_ => Random.nextBigInt(testWidth))

  // get a brand new graph every time we need it
  def graphAdd = ArithmeticGraphs.addGraph(testWidth, 0)

  def graphSub = ArithmeticGraphs.subGraph(testWidth, 0)

  def graphFull = ArithmeticGraphs.karatsubaGraph(testWidth, 0, FULL)

  def graphLow = ArithmeticGraphs.karatsubaGraph(testWidth, 0, HALF)

  def graphSquare = ArithmeticGraphs.karatsubaGraph(testWidth, 0, SQUARE)

  def graphMontMult = ArithmeticGraphs.montgomeryGraph(testWidth, 0, zprizeModulus, square = false, byLUT = false)
  def graphMontSquare = ArithmeticGraphs.montgomeryGraph(testWidth, 0, zprizeModulus, square = true, false)

  val smallData = (0 until testCaseCount * 2).map(_ => Random.nextBigInt(61))
  def graphFullSmall = ArithmeticGraphs.karatsubaGraph(61, 0, FULL)

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
    Seq(ret)
  }

  val subMetric = (yours: Seq[BigInt], golden: Seq[BigInt]) => yours.zip(golden).forall { case (x, y) =>
    if (y < 0) x - y == (BigInt(1) << testWidth) else x - (BigInt(1) << testWidth) == y
  }
  val montMetric = (yours: Seq[BigInt], golden: Seq[BigInt]) => yours.zip(golden).forall { case (x, y) => x % zprizeModulus == y % zprizeModulus }

  "addGraph" should "work" in (0 until genCount).foreach(_ => TransformTest.test(graphAdd.toTransform(golden = addGolden), data))
  "subGraph" should "work" in (0 until genCount).foreach(_ => TransformTest.test(graphSub.toTransform(golden = subGolden), data, subMetric))

  "KaratsubaGraph" should "work for full multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphFull.toTransform(golden = fullMultGolden), data))
  it should "work for low-bit multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphLow.toTransform(golden = lowMultGolden), data))
  it should "work for square multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphSquare.toTransform(golden = squareMultGolden), data.take(testCaseCount / 2).flatMap(d => Seq(d, d))))

  it should "work for the toy case" in  (0 until genCount).foreach(_ =>
    TransformTest.test(graphFullSmall.toTransform(golden = fullMultGolden), smallData))

  "montgomeryGraph" should "work for modular multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphMontMult.toTransform(golden = montMultGolden), montTestData, montMetric))
  it should "work for modular square multiplication on hardware" in (0 until genCount).foreach(_ =>
    TransformTest.test(graphMontSquare.toTransform(golden = montMultGolden),
      montTestData.grouped(4).toSeq.flatMap(group => Seq(group(0), group(0), group(2), group(3))), montMetric))
}
