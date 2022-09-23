package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class FrameBuilder(implicit staticConfig: DasStaticConfig)
  extends Component {

  val constants = staticConfig.genConstants()

  import constants._

  val flowIn = in(DasFlow(phaseUnwrapType, 1))
  val flowOut = out(DasFlow(phaseUnwrapType, 1))

  val frameCounter = Counter(256)
  when(flowIn.pulseChange)(frameCounter.increment())

  assert(!(flowIn.pulseChange && flowIn.valid))
  flowOut.valid := (flowIn.pulseChange || flowIn.valid).validAfter(1)
  val frameMark = B("11010101") // frame header
  val ret = Mux(flowIn.pulseChange, B("11111111111111111111"), flowIn.payload.head.asBits).d(1)
  flowOut.payload.assignFromBits(ret)
}
