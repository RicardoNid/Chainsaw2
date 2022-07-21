package org.datenlord
package algos

import cc.redberry.rings
import cc.redberry.rings.primes._
import cc.redberry.rings.scaladsl._
import spinal.core._

object ZPrizeMSM {

  val z = BigInt("8508c00000000001", 16)
  // (z-1)^2(z^4 - z^2 + 1)/3+z
  val baseModulus = BigInt("01ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001", 16)
  val baseRoot = BigInt("00f3c1414ef58c54f95564f4cbc1b61fee086c1fe367c33776da78169a7f3950f1bd15c3898dd1af1c104955744e6e0f", 16)
  // (z^4 - z^2 + 1)
  val scalarModulus = BigInt("12ab655e9a2ca55660b44d1e5c37b00159aa76fed00000010a11800000000001", 16)
  val scalarRoot = BigInt("0d1ba211c5cc349cd7aacc7c597248269a14cda3ec99772b3c3d3ca739381fb2", 16)

  val R = BigInt(1) << baseModulus.bitLength
  val RInverse = R.modInverse(baseModulus)
  val NPrime = ((R * RInverse - 1) / baseModulus).mod(baseModulus)

  val a = asBigInteger(0)
  val b = asBigInteger(1)
  implicit val ec: EcGroup = EcGroup(baseModulus, a, b) // y^2 = x^3 + 1

  val N = 1 << 26
  val W = 253

  def estimateWorkload(N: Int, W: Int, w: Int) = {
    val scale = BigDecimal(1) / w
    val frac1 = (BigDecimal(2).pow(w) - 1) / BigDecimal(2).pow(w) * scale
    val frac2 = (3 * w - 4).toDouble / 2 * scale
    val costStage1 = frac1 * N * W - BigDecimal(W) / w * ((1 << w) - 1)
    val costStage2 = frac2 * BigDecimal(2).pow(w) * W
    val amount = costStage1 + costStage2
    val freq = 600 * 1000 * 1000 // 600MHz
    //    println(s"amount: ${amount / 1000000}M")
    val buckets = (W / w) * (1 << w - 1) + (1 << (W % w))
    val BRAM36 = buckets.toDouble * 377 * 2 / 36 / 1000
    println(s"w = $w, $buckets buckets required, $BRAM36 BRAM36 required")
    println(s"time(600MHz, 1 PADD module): ${amount / freq}")
    //    println(s"stage1: ${stage1Cost / amount}, stage2: ${stage2Cost / amount}")
    (amount, costStage1, costStage2)
  }

  def msmOptimized(k: Seq[IntZ], p: Seq[EcPointAffine]) = {
    val add = (a: EcPointAffine, b: EcPointAffine) => a + b
    val dbl = (a: EcPointAffine) => a + a
    val problem = PippengerProblem(k.map(_.toBigInt), p, add, dbl, EcZeroAffine)
    problem.solveByOriginal._1
  }

  def msm(k: Seq[IntZ], p: Seq[EcPointAffine]) = {
    p.zip(k).map { case (affine, z) => affine * z }.reduce(_ + _)
  }

  import xilinx.VivadoUtil

  def estimateUtil() = {

    def lutMult(x: UInt, y: UInt) = {
      val product = x * y
      product.addAttribute("use_dsp", "no")
      product.d(1)
    }

    val width = 377
    //    val configMult = KaratsubaConfig(width, 34, baseMult, baseMultLatency = KaratsubaForXilinx.latency)
    //    val configLow = KaratsubaConfig(width, 34, baseMult, baseMultLatency = KaratsubaForXilinx.latency, "low")
    //    val configSquare = KaratsubaConfig(width, 34, baseMult, baseMultLatency = KaratsubaForXilinx.latency, "square")
    //    val configConstant = KaratsubaConfig(width, 34, lutMult, baseMultLatency = 1, "constant", constant = scalarModulus)

    // the last value is not bram36 but carry8
    val multUtil = VivadoUtil(32790, 23734, 232, 5235)
    val squareUtil = VivadoUtil(25450, 20924, 226, 4192)
    val lowUtil = VivadoUtil(21551, 19382, 228, 3610)
    val constantUtil = VivadoUtil(28079, 3137, 0, 4031)

    val montMultUtil = multUtil + lowUtil + constantUtil
    val montSquareUtil = squareUtil + lowUtil + constantUtil

    val fullUtil = montMultUtil * 11 + (montSquareUtil * 5)
    val board = VivadoUtil(1182000, 2364000, 6840, 147780)
    println(s"full util: $fullUtil")
    println(s"board resources: $board")
    println(s"percentage: ${fullUtil / board}")
  }

  def main(args: Array[String]): Unit = {
    //    estimateUtil()
    //    println(baseModulus.toString(2).grouped(31).map(BigInt(_, 2)).mkString(" "))
    //    println(baseModulus.toString(2).count(_ == '1'))
    //    println("start factorization")
    //    println(BigPrimes.primeFactors(baseModulus).toArray.mkString(" "))

    def construct(z: BigInt) = (z - 1).pow(2) * (z.pow(4) - z.pow(2) + 1) / 3 + z

    var top = BigInt(1) << 64
    var bottom = BigInt(1) << 63

    def current = construct((top + bottom) / 2)

    //    while (current != baseModulus) {
    //      val diff = current - baseModulus
    //      println(s"diff: ${diff}")
    //      if (diff > 0) top = (top + bottom) / 2
    //      else bottom = (top + bottom) / 2
    //    }

    //    println(s"result: ${((top + bottom) / 2).toString(16)}, diff = ${current - baseModulus}")
    //    println((z.pow(4) - z.pow(2) + 1).toString(16))
    //    println(BigPrimes.isPrime(baseModulus))

    (1 to 20).foreach(estimateWorkload(1 << 26, 253, _))
  }
}
