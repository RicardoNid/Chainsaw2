package org.datenlord
package algos

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object MontMult { // all kinds of algorithm for modular multiplication

  /** Montgomery modular multiplication
   *
   * @param N modulo
   * @return xyR^-1^ mod N
   * @see ''Modular multiplication without trial division'' [[https://www.ams.org/mcom/1985-44-170/S0025-5718-1985-0777282-X/S0025-5718-1985-0777282-X.pdf]]
   */
  def mmm(x: BigInt, y: BigInt, N: BigInt): BigInt = {

    // use number length of current crypto system
    //    val lN = nextPower2(N.bitLength).toInt
    // or, use the nearest power of 2
    val lN = N.bitLength

    // preparing parameters
    val R = BigInt(1) << lN
    val RInverse = R.modInverse(N)
    val NPrime = (R * RInverse - 1) / N

    // calculation
    val T = x * y
    val m = ((T % R) * NPrime) % R
    val t = (T + m * N) / R

    val ret = if (t >= N) t - N else t // t \in [0, 2N)
    assert(ret == (x * y * RInverse) % N)
    ret
  }
}
