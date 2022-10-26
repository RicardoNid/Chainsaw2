package org.datenlord
package comm

import spinal.core._
import spinal.lib._

case class ViterbiForwarding(trellis: Trellis, disWidth: Int, blockLength: Int)
  extends ChainsawGenerator {

  override def name = s"viterbiForwarding_dw${disWidth}_bl$blockLength".replace("-", "N")

  val maxValueForStart = 1 << (disWidth - 1)

  override def impl(dataIn: Seq[Any])  =  {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = ViterbiAlgo.viterbiForwarding(bigInts.map(_.toInt).toArray, trellis, max = maxValueForStart)
    ret.reverse.flatten.map(BigInt(_))
  }

  override var inputTypes = Seq(UIntInfo(trellis.outputBitWidth))
  override var outputTypes = Seq.fill(trellis.numStates)(UIntInfo(disWidth))

  override var inputFormat = MatrixFormat(1, blockLength)
  override var outputFormat = MatrixFormat(trellis.numStates, blockLength)
  override var latency = 0

  override def implH: ChainsawModule = new ChainsawModule(this) {

    import trellis._

    val incrementWidth = log2Up(outputBitWidth + 1) // for example, when input width is to, hamming value \in [0,2]
    val discrepancyType = HardType(UInt(disWidth bits))
    val discrepancyHalf = U(maxValueForStart, disWidth bits)

    // regs for current discrepancy
    val discrepancyHead = RegInit(discrepancyType().getZero)
    val discrepancyTail = Seq.fill(numStates - 1)(RegInit(discrepancyHalf))
    val discrepancies = Vec(discrepancyHead +: discrepancyTail)

    // convert the trellis into a sequence of minplus matrices and save them in a ROM
    val updatingMatrices = trellis.toMinplus
    val hardData: Seq[Vec[UInt]] = updatingMatrices
      .map(_.toDense)
      .map(_.map(U(_, incrementWidth bits)))
      .map(Vec(_))

    val updatingROM: Mem[Vec[UInt]] = Mem(hardData)
    updatingROM.addAttribute("rom_style", "block") // or, this would be implemented as a big MUX
    val updatingValue: Vec[UInt] = updatingROM.readAsync(uintDataIn.head)

    // combinational datapath implemented by Minplus Algebra
    // TODO: make it simpler
    // TODO: this must be accomplished in a single cycle, any better solution - interleave
    val vector: Array[Vec[UInt]] = Array(discrepancies) // 1 * numState vector
    val matrix = updatingMatrices.head.value // numState * numState matrix, we will use its shape

    val sparse2dense: Map[(Int, Int), Int] = // sparse coordinate -> dense coordinate
      Array.tabulate(numStates, numStates)((r, c) =>
        (r, c, matrix(r)(c))).flatten.filter(_._3 < MinplusMatrix.max)
        .zipWithIndex.map { case ((r, c, _), i) => (r, c) -> i }.toMap

    val discrepanciesNext = Array.tabulate(1, numStates) { (i, k) =>
      // pick up signals
      val coords = Array.tabulate(numStates)(j => ((i, j), (j, k)))
      val validCoords = coords.filter(coord => sparse2dense.isDefinedAt(coord._2))
      val pairs = validCoords.map(coord => (vector(coord._1._1)(coord._1._2), updatingValue(sparse2dense(coord._2))))
      // addition
      val afterA = pairs.map { case (dis, inc) => dis + inc } // add
      // compare & select
      val CS = (a: UInt, b: UInt) => Mux(a < b, a, b) // compare & select
      val afterCS: UInt = Vec(afterA).reduceBalancedTree(CS)
      afterCS
    }.flatten

    // control
    when(lastIn) {
      discrepancyHead.clearAll()
      discrepancyTail.foreach(_ := discrepancyHalf)
    }.otherwise(discrepancies := discrepanciesNext)
    uintDataOut := discrepancies
  }
}