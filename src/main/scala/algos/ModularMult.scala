package org.datenlord
package algos

import spinal.core._

import scala.language.postfixOps
import scala.util.Random

object ModularMult { // all kinds of algorithm for modular multiplication

  /** Montgomery modular multiplication
   *
   * @param N modulo
   * @return xyR^-1^ mod N
   * @see ''Modular multiplication without trial division'' [[https://www.ams.org/mcom/1985-44-170/S0025-5718-1985-0777282-X/S0025-5718-1985-0777282-X.pdf]]
   */
  def mmm(x: BigInt, y: BigInt, N: BigInt, lN: Int): BigInt = {
    require(N.bitLength <= lN)
    // preparing parameters
    val R = BigInt(1) << lN
    val RInverse = R.modInverse(N)
    val NPrime = (R * RInverse - 1) / N

    val golden = (x * y * RInverse) % N

    // calculation
    val T = x * y // first multiplication
    val TLow = T % R
    val m = (TLow * NPrime) % R // second multiplication
    val t = (T + m * N) / R // third multiplication

    val ret = if (t >= N) t - N else t // t \in [0, 2N)
    assert(ret == golden, s"\ngolden: $golden, \nyours : $ret")
    ret
  }

  /** barrett modular multiplication
   *
   * @see ''Langhammer, Martin and Bogdan Mihai Pasca. “Efficient FPGA Modular Multiplication Implementation.” The 2021 ACM/SIGDA International Symposium on Field-Programmable Gate Arrays (2021): n. pag.''
   */
  def barrett(x: BigInt, y: BigInt, M: BigInt, k: Int, msbOnly: Boolean = false) = {
    require(M.bitLength <= k)
    assert(M > (BigInt(1) << (k - 1)) && M < (BigInt(1) << k))
    val golden = (x * y) % M
    // pre-computation
    val MPrime = (BigInt(1) << (2 * k)) / M
    // mults
    val N = x * y // mult0 k & k
    // E is error-tolerant as we use EM in the following calculation
    val error = {
      // we take the upper bound of error, which equals the bits in a triangle
      // TODO: with a known M and MPRIME, an accurate upper bound of error can be calculated
      if (msbOnly) (1 to k + 1).map(w => BigInt(Seq.fill(w)('1').mkString(""), 2)).sum
      else BigInt(0)
    }
    val E = (MPrime * (N >> (k - 1)) - error) >> (k + 1) // mult1 k+1 & k+1, take MSBs
    val protect = 2
    // mult2 k+1 & k, take LSBs
    // subs and fine reduction
    // on hardware, T is the result of unsigned subtraction of these two operands, here, I do compensation for it
    // TODO: implement the LSB-multiplication
    val T = N - E * M

    assert(T >= golden)
    assert(T % M == golden, s"${T % M}, $golden")

    (T - golden) / M

    //    // T and golden are congruent numbers, T >= golden, T - golden \in [0, YM), where Y depends on your datapath
    //    val sub1 = T - M
    //    val sub2 = T - sub1
    //    val ret = Seq(T, sub1, sub2).find(ret => ret >= BigInt(0) && ret < M).get
    //    assert(ret == golden, s"$T, $golden")
  }

  /** high-radix montgomery modular multiplication
   */
  def hrmmm(x: BigInt, y: BigInt, N: BigInt, lN: Int, wordWidth: Int): BigInt = {
    // TODO: add the paper citation in comment
    require(N.bitLength <= lN)
    val wordCount = lN / wordWidth
    val radix = BigInt(1) << wordWidth
    val R = BigInt(1) << (wordWidth * (wordCount + 2))
    val RInverse = R.modInverse(N)

    // preparing parameters
    val NPrime = R - N.modInverse(R)
    val NBar = (NPrime % radix) * N

    // get words
    val NBarWords = {
      val temp = NBar.toWords(wordWidth)
      // TODO: find out why I wrote this requirement
      //      require(temp.length <= wordCount + 1)
      temp.padToLeft(wordCount + 1, BigInt(0))
    }
    val xWords = {
      val temp = x.toWords(wordWidth)
      require(temp.length <= wordCount + 2)
      temp.padTo(wordCount + 3, BigInt(0))
    }
    val yWords = {
      val temp = y.toWords(wordWidth)
      require(temp.length <= wordCount + 2)
      temp.padTo(wordCount + 2, BigInt(0))
    }

    val golden = (x * y * RInverse) % N

    var S = BigInt(0)
    (0 until wordCount + 3).foreach { i =>
      val q = S % radix
      S = (S + q * NBar) / radix + xWords(i) * y
    }
    val ret = S

    assert(ret % N == golden % N, s"\ngolden: $golden, \nyours : $ret")
    ret
  }

}
