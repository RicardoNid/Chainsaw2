package org.datenlord
package algos

import breeze.linalg._
import breeze.math._
import dsp.Dft
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class DftTest extends AnyFlatSpec {

  val testCount = 5

  def testFft(sizes: Seq[Int], algo: (Int, Boolean) => DenseMatrix[Complex]): Unit = {
    sizes.foreach { n =>
      val testCases = Random.RandomComplexVectors(testCount, n)
      testCases.foreach { data =>
        val error = algo(n, false) * data - Dft.dftMatrix(n, false) * data
        assert(error.forall(_.abs < 1e-3))
        val inverseError = algo(n, true) * data - Dft.dftMatrix(n, true) * data
        assert(inverseError.forall(_.abs < 1e-3))
      }
    }
  }

  "Pease Fft" should "work" in {
    testFft((1 to 7).map(1 << _), algo = Dft.peaseFftMatrix(_, 2, _))
    testFft((2 to 6 by 2).map(1 << _), algo = Dft.peaseFftMatrix(_, 4, _))
    testFft((3 to 6 by 3).map(1 << _), algo = Dft.peaseFftMatrix(_, 8, _))
  }

}
