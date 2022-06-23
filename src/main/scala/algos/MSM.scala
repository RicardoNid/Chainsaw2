package org.datenlord
package algos

import arithmetic.KaratsubaConfig
import device.KaratsubaForXilinx

import cc.redberry.rings.scaladsl._
import spinal.core._

object MSM {

  val scalarModulus = BigInt("12ab655e9a2ca55660b44d1e5c37b00159aa76fed00000010a11800000000001", 16)
  val scalarRoot = BigInt("0d1ba211c5cc349cd7aacc7c597248269a14cda3ec99772b3c3d3ca739381fb2", 16)

  val baseModulus = BigInt("01ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001", 16)
  val baseRoot = BigInt("00f3c1414ef58c54f95564f4cbc1b61fee086c1fe367c33776da78169a7f3950f1bd15c3898dd1af1c104955744e6e0f", 16)

  val a = asBigInteger(0)
  val b = asBigInteger(1)
  implicit val ec = EccGroup(baseModulus, a, b)

  def msm(k: Seq[IntZ], p: Seq[EccPointAffine]) = {
    val add = (a: EccPointAffine, b: EccPointAffine) => a + b
    val dbl = (a: EccPointAffine) => a + a
    val problem = PippengerProblem(k.map(_.toBigInt), p, add, dbl, EccZeroAffine)
    problem.solveByOriginal._1
  }

  import xilinx.VivadoUtil

  def estimateUtil() = {

    def baseMult(x: UInt, y: UInt) = {
      val core = KaratsubaForXilinx()
      core.dataIn.payload := Vec(x.resized, y.resized)
      core.dataIn.valid := True
      core.dataOut.payload
    }

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
    estimateUtil()
  }
}
