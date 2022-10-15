package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class MatIntrlvCore(row: Int, col: Int, width: Int)
  extends ChainsawGenerator {

  // TODO: pipeline for this module

  override def name = s"matintrlv_r${row}_c${col}_w$width"

  override val impl: Seq[Any] => Seq[Any] = CommonAlgos.matIntrlv(row, col, _)

  override var inputTypes = Seq.fill(col)(UIntInfo(width))
  override var outputTypes = Seq.fill(row)(UIntInfo(width))

  val period = row max col
  override var inputFormat = MatrixFormatAddBubble(col, row, period - row)
  override var outputFormat = MatrixFormatAddBubble(row, col, period - col)
  override var latency = row + 3

  override def implH: ChainsawModule = new ChainsawModule(this) {

    /** --------
     * PIPO state machine
     * -------- */
    val readPointer: Bool = RegInit(False)
    val writePointer: Bool = RegInit(False)
    val counterIn: Counter = CounterFreeRun(row)
    val counterOut: Counter = CounterFreeRun(col)
    val writeRefresh = lastIn.validAfter(1)
    val readRefresh = lastIn.validAfter(row + 1)
    when(writeRefresh)(counterIn.clear())
    when(readRefresh.validAfter(2))(counterOut.clear())
    when(writeRefresh)(writePointer := ~writePointer)
    when(readRefresh)(readPointer := ~readPointer)

    /** --------
     * RAM declaration
     * we need a square matrix to contain all data, when row != col, this introduced redundancy;
     * besides, for depth, we use a power of 2, again introduced redundancy;
     * but this is suitable for FPGAs as it simplifies the control logic
     * -------- */
    val ramCount = row max col
    val addrWidth: Int = log2Up(row max col)
    val ramDepth = 1 << addrWidth
    // ping-pong was implemented by using different address range of the same dual-port ram
    val rams = Seq.fill(ramCount)(Mem(Bits(width bits), ramDepth << 1))

    /** --------
     * datapath
     * 1. dataIn -> padded -> rotated
     * 2. rotated -> RAM, by natural addrs
     * 3. RAM -> rotatedOut, by generated addrs
     * 4. rotatedOut -> paddedOut -> dataOut
     * -------- */

    val zero = B(0, width bits)
    // -> padded
    val dataInPadded = Vec(dataIn.padTo(ramCount, zero))
    // -> rotated latency +1(pipeline)
    val dataInShifted = cloneOf(dataInPadded)

    def rotateLeft(i: Int): IndexedSeq[Bits] = dataInPadded.takeRight(ramCount - i) ++ dataInPadded.take(i)

    switch(counterIn.value) {
      (0 until row).foreach(i => is(U(i))(dataInShifted := rotateLeft(i).map(_.d(1)))) // rotate to match the ports
      if (!isPow2(row)) default(dataInShifted.assignDontCare()) // when default is needed
    }
    // -> RAM latency +row(wait till all inputs are ready)
    rams.zip(dataInShifted).foreach { case (ram, data) =>
      ram.write(
        address = writePointer.asUInt @@ counterIn.value.resize(addrWidth), // using write pointer as the MSB
        data = data, enable = validIn.validAfter(1))
    }
    // read addrs generation(by rotation)
    val initValues = Vec((0 +: (1 until ramCount).reverse).map(U(_, addrWidth bits)))
    val readAddrs = RegInit(initValues)
    when(readRefresh)(readAddrs := initValues).otherwise(readAddrs := readAddrs.rotateRight(1))
    // -> rotatedOut
    val dataOutShifted = Vec(
      rams.zip(readAddrs).map { case (ram, addr) => // zip ports with addrs
        ram.readSync(address = readPointer.asUInt @@ addr) // using read pointer as MSB
      }.padTo(ramCount, zero))
    // -> paddedOut latency +1(pipeline)
    val dataRemapped = Vec(dataOutShifted.head +: dataOutShifted.tail.reverse)
    val dataOutPadded = cloneOf(dataRemapped)

    def rotateRight(i: Int): IndexedSeq[Bits] = dataRemapped.takeRight(i) ++ dataRemapped.take(ramCount - i)

    switch(counterOut.value) {
      (0 until col).foreach(i => is(U(i))(dataOutPadded := rotateRight(i).map(_.d(1))))
      if (!isPow2(col)) default(dataOutPadded.assignDontCare())
    }
    // -> dataOut
    dataOut := dataOutPadded.take(row) // drop the padded part
  }
}

case class MatIntrlv(row: Int, col: Int, width: Int, pF: Int)
  extends ChainsawGenerator {

  val mode = { // TODO: more modes
    if (pF % row == 0 && pF % col == 0 && (row * col) % pF == 0) 0
    else -1
  }

  require(mode >= 0, "your configuration hasn't been implemented yet")

  override def name = s"matintrlv_r${row}_c${col}_w${width}_sw$pF"

  override val impl: Seq[Any] => Seq[Any] = CommonAlgos.matIntrlv(row, col, _)

  override var inputTypes = Seq.fill(pF)(UIntInfo(width))
  override var outputTypes = Seq.fill(pF)(UIntInfo(width))

  val period = row max col
  override var inputFormat = MatrixFormat(pF, row * col / pF)
  override var outputFormat = MatrixFormat(pF, row * col / pF)

  // packing parameters
  val packRow = pF / col
  val packCol = pF / row
  val packSize = packRow * packCol // (intersection size of input and output)
  val squareSize = pF / packSize
  val packWidth = packSize * width
  val coreGen = MatIntrlvCore(squareSize, squareSize, packWidth)

  override var latency = coreGen.latency

  override def implH: ChainsawModule = new ChainsawModule(this) {

    // packing input
    val dataInRearranged = CommonAlgos.matIntrlv(packRow, col, dataIn)
    val dataInPacked = Vec(dataInRearranged.grouped(packSize).toSeq.map(_.asBits()))

    // connecting the core
    val core = coreGen.implH
    core.dataIn := dataInPacked
    core.validIn := validIn
    core.lastIn := lastIn
    val coreOut = core.dataOut

    def unpack(in: Seq[Bits]): Vec[Bits] = {
      val ret = cloneOf(dataOut)
      (0 until row / packRow).foreach { packId =>
        (0 until packCol).foreach { packColId =>
          (0 until packRow).foreach { packRowId =>
            val id = packColId * row + packId * packRow + packRowId
            val idInPack = packColId * packRow + packRowId
            ret(id).assignFromBits(in(packId).subdivideIn(packSize slices)(idInPack))
          }
        }
      }
      ret
    }

    dataOut := unpack(coreOut)
  }
}
