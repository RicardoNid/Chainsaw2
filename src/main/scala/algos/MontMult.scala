package org.datenlord
package algos

import spinal.core._
import scala.language.postfixOps

object MontMult { // all kinds of algorithm for modular multiplication

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

  /** high-radix montgomery modular multiplication
   */
  def hrmmm(x: BigInt, y: BigInt, N: BigInt, lN:Int, wordWidth: Int): BigInt = {
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
      require(temp.length <= wordCount + 1)
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
