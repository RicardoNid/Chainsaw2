package org.datenlord
package ip.das

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class SpatialExtractor(implicit staticConfig: DasStaticConfig)
  extends Component {

  val constants = staticConfig.genConstants()

  import constants._

  val flowIn = in(DasFlow(phaseUnwrapType, 1))
  val flowOut = out(DasFlow(phaseUnwrapType, 1))

  val positionIn = in UInt (log2Up(spatialPointsMax) bits)
  val position = RegNextWhen(positionIn, flowIn.modeChange, init = U(0))

  val positionCounter = Counter(spatialPointsMax)
  when(flowIn.pulseChange)(positionCounter.clear())
  when(flowIn.valid)(positionCounter.increment())

  flowOut := flowIn.pipeWith(flowIn.payload.d(1), 1)
  flowOut.valid.allowOverride
  flowOut.valid := (positionCounter === position && flowIn.valid.validAfter(1))

}
