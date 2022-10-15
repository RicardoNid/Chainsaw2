package org.datenlord
package zprize

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/** High-throughput general interlever that implements matrix interleaving with any given parameters
 *
 * @param row         the semantic is the same as row of Matlab matintrlv
 * @param col         the semantic is the same as row of Matlab matintrlv
 *                    for matrix interleaver, the de-interleaver is an interleaver that exchange the original row and col
 * @param streamWidth number of elements in/out per cycle
 * @param bitWidth    bit width of each element
 * @see ''R. Chen and V. K. Prasanna, "Energy-efficient architecture for stride permutation on streaming data," 2013 International Conference on Reconfigurable Computing and FPGAs (ReConFig), 2013, pp. 1-7, doi: 10.1109/ReConFig.2013.6732285.''
 *
 *      [[https://ieeexplore.ieee.org/document/6732285]]
 *
 *      although my implementation is similar,I proposed it all by myself
 */
case class StridePermutation(row: Int, col: Int, streamWidth: Int, bitWidth: Int)
  extends ChainsawGenerator {

  val baseWidth = gcd(row, col)

  override def name = s"StridePermutation_r${row}_c${col}_p${streamWidth}_w$bitWidth"

  override val impl = (dataIn: Seq[Any]) => dataIn.asInstanceOf[Seq[BigInt]]
    .grouped(col).toSeq.transpose.flatten // TODO: row or column?

  override var inputTypes = Seq.fill(streamWidth)(UIntInfo(bitWidth))
  override var outputTypes = Seq.fill(streamWidth)(UIntInfo(bitWidth))
  val period = row * col / streamWidth
  val format = MatrixFormat(streamWidth, period)
  override var inputFormat = format
  override var outputFormat = format

  override def implH = ???

  // for implNaiveH
  val frameSize = row * col
  //  val intrlvS2P = S2P(streamWidth / baseWidth, frameSize / baseWidth, baseWidth)
  //  val intrlvP2S = P2S(frameSize / baseWidth, streamWidth / baseWidth, baseWidth)
  // TODO: take advantage of baseWidth
  val intrlvS2P = S2P(streamWidth, frameSize, 1)
  val intrlvP2S = P2S(frameSize, streamWidth, 1)

  def matIntrlv(dataIn: Seq[Bits])
  = dataIn.grouped(col).toSeq.transpose.flatten

  override var latency = intrlvS2P.latency + intrlvP2S.latency

  override def implNaiveH: ChainsawModule = new ChainsawModule(this) {

    val s2p = intrlvS2P.implDut
    val p2s = intrlvP2S.implDut

    s2p.dataIn.fragment := dataIn
    s2p.dataIn.last := lastIn
    s2p.dataIn.valid := validIn

    s2p.dataOut >> p2s.dataIn
    p2s.dataIn.fragment.allowOverride
    p2s.dataIn.fragment := matIntrlv(s2p.dataOut.payload)

    dataOut := p2s.dataOut.fragment
  }
}