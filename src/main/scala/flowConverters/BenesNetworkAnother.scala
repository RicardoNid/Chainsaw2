package org.datenlord
package flowConverters

import flowConverters.Permutation
import spinal.core._

import scala.language.postfixOps

/** configuration for periodic Benes network
 *
 * @param N              size of input/output vector
 * @param permutations   a group of permutations executed by the network periodically
 * @param stagesPerCycle number of cascaded switches in between pipeline registers
 * @see ''MATHEMATICS FOR COMPUTER SCIENCE'' Chapter 11
 */
case class BenesNetworkAnother(N: Int, permutations: Seq[Permutation], bitWidth: Int, stagesPerCycle: Int)
  extends ChainsawGenerator {

  override def name = s"benes_N${N}_w${bitWidth}_perm${permutations.hashCode()}".replace('-', 'N')

  val n = permutations.head.size

  override val impl = (dataIn: Seq[Any]) => {
    val segments = dataIn.asInstanceOf[Seq[BigInt]].grouped(n).toSeq
    segments.zip(permutations).flatMap { case (ints, permutation) => permutation.permute(ints) }
  }

  override var inputTypes = Seq.fill(n)(UIntInfo(bitWidth))
  override var outputTypes = Seq.fill(n)(UIntInfo(bitWidth))

  override var inputFormat = MatrixFormat(n, permutations.length)
  override var outputFormat = MatrixFormat(n, permutations.length)
  override var latency = 1

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val controlROM = Mem(permutations.map(Benes.getControlForHard))
    val controlIn: Seq[Seq[Bool]] = controlROM.readAsync(localCounter.value).map(_.asBools)

    def switch22(a: Bits, b: Bits, switch: Bool): Seq[Bits] = Seq(Mux(switch, b, a), Mux(switch, a, b))

    dataOut := Benes.doBenes(dataIn, BenesControl(controlIn), switch22).map(_.d(1))
  }
}