package org.datenlord
package flowConverters

import breeze.linalg._
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Stream permutation module for arbitrary permutation, based on RAMs
 *
 * @param permutation permutation defined by a sequence of integers, can be generated by Random.shuffle on (0 until n) sequence
 * @param bitWidth    bit width of elements in input & output vector
 * @see ''Automatic Generation of Streaming Datapaths for Arbitrary Fixed Permutations, Peter A. Milder, James C. Hoe, and Markus P¨uschel''
 */
case class PermutationByRamConfig[T <: Data](permutation: Seq[Int], override val spaceFold: Int, dataType: HardType[T])
  extends TransformBase {

  require(N % spaceFold == 0)
  val N = permutation.length
  val w = N / spaceFold
  val period = N / w

  val memDepth = 2 * period
  val addrWidth = log2Up(N / w)

  override val size = (N, N)

  val (permutations, readAddr, writeAddr) = if (N > w) getControls else (null, null, null)

  val networkConfig = if (N > w) BenesNetworkConfig(w, permutations, 2, dataType) else null

  override def latency = if (N > w) period * 2 + networkConfig.latency else 1

  override def spaceFolds = factors(N)

  override def getConfigWithFoldsChanged(spaceFold: Int, timeFold: Int) =
    PermutationByRamConfig(permutation, spaceFold, dataType)

  override def impl(dataIn: Seq[Any]) = permutation.map(dataIn(_))

  def getMappingMatrix = {
    val mappingMatrixContent = Array.tabulate(w, w) { (i, j) =>
      permutation.zipWithIndex.count { case (out, in) =>
        (out % w == j) && (in % w == i)
      }
    }
    new DenseMatrix(w, w, mappingMatrixContent.flatten)
  }

  // get permutation matrices from mapping matrix by brute force algorithm
  // TODO: better algorithm
  def getDecomposition(mapping: DenseMatrix[Int]) = {
    val n = mapping.rows
    logger.info(s"decomposing mapping $n X $n matrix\n${mapping.toString()}")
    require(mapping.rows == mapping.cols)

    // generate all permutations of length n, implemented as a iterator(delayed)
    def perms(n: Int) = (0 until n).permutations

    // build permutation matrix from a permutation
    def perm2Matrix(perm: Seq[Int]) = {
      val n = perm.length
      val content = Array.tabulate(n, n)((i, j) => if (perm(i) == j) 1 else 0)
      new DenseMatrix(n, n, content.flatten)
    }

    var current = mapping
    val ret = ArrayBuffer[Seq[Int]]()
    // brute force(traversal) algorithm
    perms(n).zipWithIndex.foreach { case (perm, i) =>
      val permMatrix = perm2Matrix(perm)
      // algorithm 6 line 3-6
      while ((current - permMatrix).forall(_ >= 0)) {
        current = current - permMatrix
        ret += perm
      }
    }
    ret
  }

  /** get control bits stored in R,W & T by decomposing mapping matrix
   *
   */
  def getControls = {
    // build the mapping matrix according to definition 2
    val mappingMatrix = getMappingMatrix
    val permutations = getDecomposition(mappingMatrix).map(perm => Permutation(perm))
    val readAddr = ArrayBuffer[Seq[Int]]() // read addr of M_0
    val writeAddr = ArrayBuffer[Seq[Int]]() // write addr of M_1
    // remained elements
    val elems = mutable.Set(0 until N: _*)
    // decomposition of mapping matrix
    permutations.foreach { case (perm) =>
      // algorithm 6 line7
      val elem = (0 until w).map { i =>
        elems.find(x => (x % w == i) && (permutation.indexOf(x) % w == perm.permuted.indexOf(i))).get
      }
      elems --= elem
      // algorithm 6 line9
      readAddr += elem.map(_ / w)
      // algorithm 6 line10
      writeAddr += elem.indices.map(index => permutation.indexOf(elem(perm.permuted(index))) / w)
    }
    (permutations, readAddr, writeAddr)
  }

  override def implH = PermutationByRam(this)
}

case class PermutationByRam[T <: Data](config: PermutationByRamConfig[T])
  extends TransformModule[T, T] {

  import config._

  // I/O
  override val dataIn = slave Flow Fragment(Vec(dataType, w))
  override val dataOut = master Flow Fragment(Vec(dataType, w))
  if (N > w) {
    // components
    val network = BenesNetwork(networkConfig)
    val R = Mem(readAddr.map(seq => Vec(seq.map(U(_, addrWidth bits)))))
    val W = Mem(writeAddr.map(seq => Vec(seq.map(U(_, addrWidth bits)))))
    val M0, M1 = Seq.fill(w)(Mem(dataType, memDepth))
    // controls
    val counter = autoInputCounter()
    val state = counter.value
    val controlLatency = networkConfig.latency % period
    val stateAfterN = state.d(controlLatency)
    val pingPongM0 = Reg(UInt(1 bits))
    when(counter.willOverflow)(pingPongM0 := ~pingPongM0)
    val pingPongM1 = Reg(UInt(1 bits))
    when(counter.willOverflow.d(controlLatency))(pingPongM1 := ~pingPongM1)
    // datapath
    // step 1 input -> M0
    M0.zip(dataIn.fragment).foreach { case (port, data) => port.write(state @@ pingPongM0, data) }
    // step 2 M0 -> data
    val readAddrs = R.readAsync(state)
    val afterM0 = Vec(M0.zip(readAddrs).map { case (port, addr) => port.readAsync(addr @@ ~pingPongM0) })
    // step 3 data -> network -> data
    network.dataIn.fragment := afterM0
    network.dataIn.valid := dataIn.valid.validAfter(period)
    network.dataIn.last := dataIn.last.validAfter(period)
    val afterN = network.dataOut.fragment
    // step 4 data -> M1
    val writeAddrs = W.readAsync(stateAfterN)
    M1.zip(writeAddrs.zip(afterN)).foreach { case (port, (addr, data)) =>
      port.write(addr @@ pingPongM1, data)
    }
    // step 5 M1 -> output
    dataOut.fragment := Vec(M1.map(port => port.readAsync(stateAfterN @@ ~pingPongM1)))

  }
  else {
    val reordered = Vec(permutation.map(i => dataIn.fragment(i)))
    dataOut.fragment := RegNext(reordered)
  }
  autoLast()
  autoValid()
}