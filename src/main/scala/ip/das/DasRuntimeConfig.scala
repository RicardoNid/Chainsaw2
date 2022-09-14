package org.datenlord
package ip.das

case class DasRegValues(
                         pulsePoints: Int,
                         gaugePoints: Int,
                         spatialPoints: Int,
                         pulsePeriod: Int,
                         gain: Int
                       )

case class DasRuntimeConfig(
                             gaugeLength: Double,
                             probeLength: Double,
                             bandWidth: Double,
                             gain: Int
                           ) {
  def genRegValues(staticConfig: DasStaticConfig) = {

    import staticConfig._

    def pulseFreq = c / ((probeLength + 0.1) * 1e3 * 2)

    def pulsePoints = (samplingFreq / pulseFreq).ceil.toInt

    def gaugePoints = (gaugeLength * 2 / c * samplingFreq).ceil.toInt

    DasRegValues(
      pulsePoints = pulsePoints,
      gaugePoints = gaugePoints,
      spatialPoints = (pulsePoints / gaugePoints).ceil.toInt,
      pulsePeriod = (sigProFreq / pulseFreq).ceil.toInt,
      gain = gain
    )
  }
}
