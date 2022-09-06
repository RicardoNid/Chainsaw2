package org.datenlord
package arithmetic

import dfg.RingDag

object Barrett377 {

  def apply(k: Int, M: BigInt): RingDag = {

    require(M.bitLength <= k)

    val MPrime = (BigInt(1) << (2 * M.bitLength)) / M
    logger.info(s"width of MPrime = ${MPrime.bitLength}")
    val golden = (dataIn: Seq[BigInt]) => Seq(dataIn.product % M)

    implicit val graph: RingDag = new RingDag("BarrettGraph", golden)

    // first multiplication
    val A = graph.addInput("A", k + 1)
    val B = graph.addInput("B", k + 1)
    val N = Karatsuba377().asRingOp(Seq(A, B)).head
      .resize(754)
    //    val z = graph.addOutput("Z", k * 2)
    //    z := N

    val NHigh = N.splitAt(k - 1)._1 // k+1
    logger.info(s"width of NHigh = ${NHigh.width}")
    val E = BcmConfig(MPrime, k + 1, MsbMultiplier, widthTake = k + 1, useCsd = true).asRingOp(Seq(NHigh)).head // k+1
    //    val z = graph.addOutput("Z", k + 1)
    //    z := E

    val ME = BcmConfig(M, k + 1, LsbMultiplier, widthTake = k + 2, useCsd = true).asRingOp(Seq(E)).head
    val z = graph.addOutput("Z", k + 2)
    z := ME

    val NLow = N.splitAt(k + 1)._2
    //    val ret = NLow - ME

    graph
  }

}
