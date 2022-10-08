package org.datenlord
package arithmetic

import dfg.RingDag

/** fully pipelined Barrett modular multiplication for ZPRIZE
 *
 * @see ''Langhammer, Martin and Bogdan Mihai Pasca. “Efficient FPGA Modular Multiplication Implementation.” The 2021 ACM/SIGDA International Symposium on Field-Programmable Gate Arrays (2021): n. pag.''
 */
object Barrett377 {

  def apply(k: Int, M: BigInt): RingDag = {

    require(M.bitLength <= k)

    val MPrime = (BigInt(1) << (2 * M.bitLength)) / M
    val golden = (dataIn: Seq[BigInt]) => Seq(dataIn.product % M)

    implicit val graph: RingDag = new RingDag("BarrettGraph", golden)

    val fullMult = Karatsuba377().reconstruct().asRingOp(_)
    val msbBcm = BcmConfig(MPrime, k + 1, MsbMultiplier, widthTake = k + 1, useCsd = true).asRingOp(_)
    val lsbBcm = BcmConfig(M, k + 1, LsbMultiplier, widthTake = k + 2, useCsd = true).asRingOp(_)
    val sub = CpaConfig(k + 2, BinarySubtractor, 1).asRingOp(_)
    val upperBound = 2
    val rec = FineReduction(M, upperBound).asRingOp(_)

    // first multiplication
    val A = graph.addInput("A", k)
    val B = graph.addInput("B", k)
    val N = fullMult(Seq(A, B)).head // 2k
    // bits
    // second multiplication
    val NHigh = N.splitAt(k - 1)._1 // k+1 bits, high
    val E = msbBcm(Seq(NHigh)).head // k+1 bits
    // third multiplication
    val ME = lsbBcm(Seq(E)).head // k+2 bits
    // subtraction
    val NLow = N.splitAt(k + 2)._2 // k+2 bits, low
    val T = sub(Seq(NLow, ME)).head
    // fine reduction
    val R = rec(Seq(T)).head

    val z = graph.addOutput("Z", k) //
    z := R

    graph
  }

}
