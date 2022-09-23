package org.datenlord
package ip.das

case class DasRegValues(
                         pulsePoints: Int,
                         gaugePoints: Int,
                         spatialPoints: Int,
                         pulsePeriod: Int,
                         gain: Int,
                         position: Int
                       )

case class DasRuntimeConfig(
                             gaugeLength: Double,
                             probeLength: Double,
                             bandWidth: Double,
                             gain: Int, // 31 - real gain
                             probePosition: Double
                           ) {
  def genRegValues(staticConfig: DasStaticConfig) = {

    import staticConfig._
    val constants = staticConfig.genConstants()
    import constants._

    def pulseFreq = c / ((probeLength + 0.1) * 1e3 * 2)

    def pulsePoints = (samplingFreq / pulseFreq).ceil.toInt.nextMultiple(subFilterCount)

    def gaugePoints = (gaugeLength * 2 / c * samplingFreq).ceil.toInt.nextMultiple(subFilterCount)

    def position = (probePosition / gaugeLength).ceil.toInt

    DasRegValues(
      pulsePoints = pulsePoints,
      gaugePoints = gaugePoints,
      spatialPoints = (pulsePoints.toDouble / gaugePoints.toDouble).ceil.toInt,
      pulsePeriod = (sigProFreq / pulseFreq).ceil.toInt,
      gain = gain,
      position = position
    )
  }
}
