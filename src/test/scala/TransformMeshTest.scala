package org.datenlord

import breeze.linalg.DenseMatrix
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class TransformMeshTest extends AnyFlatSpec {

  "mesh" should "work with no reuse" in {
    val config = flowConverters.PermutationByRamConfig(Seq(1, 2, 3, 0), 4, 4)
    val data = Seq(0, 1, 2, 3, 4, 5, 6, 7).map(BigInt(_))
    val mesh = config ⊗ 2 ∏ 2

    // test for a 2 * 2 mesh
    TransformTest.test(mesh.implForTest(UInt(4 bits), UInt(4 bits)), data)
  }

  // base which is space foldable, latency = 3
  val dataType0 = HardType(ComplexFix(3 exp, -12 exp))
  val coeff0 = Random.RandomSequences(1, 2, Random.nextComplex).head
  val configSF = arithmetic.DiagonalConfig(coeff0, dataType0, dataType0, spaceFold = 1)

  // base which is time foldable, latency = 3
  val dataType1 = HardType(SFix(5 exp, -10 exp))
  val coeff1 = DenseMatrix.rand[Double](4, 4)
  val configTF = arithmetic.AlgebraConfig(coeff1, dataType1, dataType1, timeFold = 1) // latency = 3

  it should "work with space reuse, time reuse & space fold reuse when fifo exist" in {
    val mesh = configSF.⊗(2, 1).⊗(2).∏(2).withReuse(Reuse(4, 2, 2, 1))
    assert(mesh.flowFormat.queue < mesh.flowFormat.inQueue)
    TransformTest.test(mesh.implForTest(dataType0, dataType0), mesh.getRandomDataIn(Random.nextComplex), Metric.ComplexAbs(1e-2))
  }

  it should "work with space reuse, time reuse & space fold reuse when bubble exist" in {
    val mesh = configSF.⊗(2, 1).⊗(2).∏(4).withReuse(Reuse(2, 2, 2, 1))
    assert(mesh.flowFormat.queue > mesh.flowFormat.inQueue && mesh.flowFormat.util < 1)
    TransformTest.test(mesh.implForTest(dataType0, dataType0), mesh.getRandomDataIn(Random.nextComplex), Metric.ComplexAbs(1e-2))
  }

  it should "work with space reuse, time reuse & time fold reuse when fifo exist" in {
    val mesh = configTF.⊗(2, 1).⊗(2).∏(2).withReuse(Reuse(4, 2, 1, 2))
    assert(mesh.flowFormat.queue < mesh.flowFormat.inQueue)
    TransformTest.test(mesh.implForTest(dataType1, dataType1), mesh.getRandomDataIn(Random.nextDouble), Metric.DoubleAbs(1e-1))
  }

  it should "work with space reuse, time reuse & time fold reuse when bubble exist" in {
    val mesh = configTF.⊗(2, 1).⊗(2).∏(4).withReuse(Reuse(2, 2, 1, 2))
    assert(mesh.flowFormat.queue > mesh.flowFormat.inQueue && mesh.flowFormat.util < 1)
    TransformTest.test(mesh.implForTest(dataType1, dataType1), mesh.getRandomDataIn(Random.nextDouble), Metric.DoubleAbs(5e-1))
  }

  "searching algorithm" should "be able to find best reuse parameters" in {
    val mesh = configTF.⊗(2, 1).⊗(2).∏(4).withReuse(Reuse(2, 2, 1, 2))
    def testTargetThroughput(target:Double) = {
      val optimized = mesh.fitTo(target)
      assert(optimized.flowFormat.throughput >= target)
      TransformTest.test(optimized.implForTest(dataType1, dataType1), optimized.getRandomDataIn(Random.nextDouble), Metric.DoubleAbs(5e-1))
    }
    (1 to 10).map(_.toDouble / 10).foreach(testTargetThroughput)
  }
}