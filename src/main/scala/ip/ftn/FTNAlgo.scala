package org.datenlord
package ip.ftn

import breeze.math._
import scala.util.Random

case class FtnConfig(dataCarrierPositions: Seq[Int]) {

  val subCarrierCount = 256 // sub carrier count in total
  val bitsPerQAMSymbol = 4
  val dataSymbolsPerFrame = 8
  val preambleSymbolsPerFrame = 2

  val fftCount = subCarrierCount * 2

  val n1 = dataCarrierPositions.length

  val dataBitsPerFrame = dataSymbolsPerFrame * subCarrierCount * bitsPerQAMSymbol
  val preambleBitsPerFrame = preambleSymbolsPerFrame * subCarrierCount * bitsPerQAMSymbol

}

case class FTNAlgo(config: FtnConfig) {

  import config._

  def bitGen(bitNumber: Int)(implicit seed: Int) = {
    Random.setSeed(seed)
    Random.nextBits(bitNumber)
  }

  def dataGen(implicit seed: Int) = bitGen(dataBitsPerFrame)

  def preambleGem(implicit seed: Int) = bitGen(preambleBitsPerFrame)

  def qammodWithAlloc(data: Seq[Int], bitAlloc: Seq[Int], powAlloc:Seq[Double]) = {

  }

  def multiplex(data:Seq[Complex], n1:Int, n2:Int) = {
    require(data.length == n1)

  }
}
