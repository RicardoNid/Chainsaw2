package org.datenlord
package ip.convenc

import spinal.core._
import spinal.lib._

case class Convenc128FTN() extends Component {

  val dataIn = in Bits (128 bits)
  val dataOut = out Bits (256 bits)

  // as parallelism >= block size & termination mode is termination, states are not needed

  val string171 = "1111001"
  val string133 = "1011011"
  val convenc = (bools: Seq[Bool]) =>
    bools.zip(string171).filter(_._2 == '1').map(_._1).xorR ##
      bools.zip(string133).filter(_._2 == '1').map(_._1).xorR

  val ret = (B"000000" ## dataIn).asBools
    .sliding(7).toSeq
    .map(convenc).asBits()

  dataOut := RegNext(ret)

  val latency = 1
}
