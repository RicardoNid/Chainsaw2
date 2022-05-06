package org.datenlord
package ip.fft

import dataFlow._

/**
 * by default, this module takes signed normalized input within [-1, 1)
 */
case class PeaseFftConfig(N: Int, width: Int, fold: Int) extends TransformConfig {

  override def latency = ???

  override def inputFlow = ???

  override def outputFlow = ???

  override def transform(dataIn: Seq[BigInt]) = ???
}

case class PeaseFft(config: PeaseFftConfig) {



}
