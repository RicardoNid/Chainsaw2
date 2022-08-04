package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core
import spinal.core._

import scala.util.Random

class PipelinedBigAdderTest extends AnyFlatSpec {

  val testCount = 1000
  val width = 377
  val baseWidth = 127

  val data = (0 until testCount * 2).map(_ => Random.nextBigInt(width))
  val config = PipelinedBigAdderConfig(width, baseWidth)

  "PipelinedBigAdder" should "work by algo" in assert(config.graph.evaluateS(data.take(2)).head == data.take(2).sum)

  it should "work" in TransformTest.test(config.implH, data)

  case class BaseAddition(width: Int) extends Component {
    val a, b = in UInt (width bits)
    val carry = in UInt (1 bits)
    val c = out UInt (width + 1 bits)
    c := (a.d(1) +^ b.d(1) + carry).d(1)
  }

  ignore should "synth and compare with plain implementation" in {
    val config0 = PipelinedBigAdderConfig(377, 127)
    val config1 = PipelinedBigAdderConfig(377, 128)
    VivadoSynth(config0.implH, "bigAdd_377_127")
    VivadoSynth(config1.implH, "bigAdd_377_128")
  }
}
