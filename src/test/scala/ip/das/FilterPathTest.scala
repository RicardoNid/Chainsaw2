package org.datenlord
package ip.das

import breeze.numerics.constants._
import breeze.numerics.{cos, sin}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FilterPathTest extends AnyFlatSpec {

  val testCount = 30
  def getOne: Double = Random.nextDouble() * 2 - 1 // [-1,1]

  def getGroup: Seq[Double] = {
    val phase = getOne * 2 * Pi / 2
    val length = getOne.abs
    Seq(cos(phase) * length, sin(phase) * length)
  }

  val validGroups = Seq.fill(testCount)(getGroup).flatten

  matlabEngine.eval("load /home/ltr/sysudas/code/matlab/das500MHz.mat")
  matlabEngine.eval("dataIn = Channel_1.Data(1:1250);") // a pulse
  //  matlabEngine.eval("dataIn = Channel_1.Data(1:125000);") // a pulse
  matlabEngine.eval("dataIn = double(dataIn) ./ 32768.0;")
  val data = matlabEngine.getVariable[Array[Double]]("dataIn")
  val dasConfig = DasConfig(samplingFreq = 1e9)

  "filter path" should "work" in TransformTest.test(FilterPathConfig(dasConfig).implH, validGroups, name = "filterPathTest")

}
