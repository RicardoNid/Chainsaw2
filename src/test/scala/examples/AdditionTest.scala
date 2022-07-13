package org.datenlord
package examples

import xilinx.VivadoReport

import org.scalatest.flatspec.AnyFlatSpec
import arithmetic.{PipelinedBigAdder, PipelinedBigAdderConfig}

class AdditionTest extends AnyFlatSpec {

  def testAddersOfWidths(widths: Seq[Int]): Unit = {
    val reports: Seq[(Int, VivadoReport)] = widths.map(width => width -> VivadoSynth(Addition(width), name = s"additionTest_$width"))
    reports.foreach { report =>
      println(s"${report._1} -> efficiency of LUT = ${report._2.LUT / report._1.toDouble}, CARRY8 = ${report._2.CARRY8 / report._1.toDouble}, FMax = ${report._2.Frequency / 1e6}MHz ")
    }
  }

  def testSubtractersOfWidths(widths: Seq[Int]): Unit = {
    val reports: Seq[(Int, VivadoReport)] = widths.map(width => width -> VivadoSynth(Subtraction(width), name = s"additionTest_$width"))
    reports.foreach { report =>
      println(s"${report._1} -> efficiency of LUT = ${report._2.LUT / report._1.toDouble}, CARRY8 = ${report._2.CARRY8 / report._1.toDouble}, FMax = ${report._2.Frequency / 1e6}MHz ")
    }
  }

  def testBigAddersOfWidths(widths: Seq[Int]): Unit = {
    val reports: Seq[(Int, VivadoReport)] = widths.map(width => width -> VivadoSynth(PipelinedBigAdder(PipelinedBigAdderConfig(width, 127)), name = s"additionTest_$width"))
    reports.foreach { report =>
      println(s"${report._1} -> efficiency of LUT = ${report._2.LUT / report._1.toDouble}, CARRY8 = ${report._2.CARRY8 / report._1.toDouble}, FMax = ${report._2.Frequency / 1e6}MHz ")
    }
  }

  //  "this test" should "get cost and fmax of adders of different widths" in testAddersOfWidths(Seq(8-1, 16-1, 32-1, 64-1, 128-1, 256-1, 512-1, 1024-1))
  "this test" should "get cost and fmax of subtracters of different widths" in testSubtractersOfWidths(Seq(8 - 1, 16 - 1, 32 - 1))
  it should "get cost and fmax of adders of different widths" in testAddersOfWidths(Seq(256 - 1, 512 - 1, 1024 - 1))
  //  it should "get cost and fmax of subtracters of different widths" in testSubtractersOfWidths(Seq(256 - 1, 512 - 1, 1024 - 1))
  it should "get cost and fmax of pipelined adders of different widths" in testBigAddersOfWidths(Seq(256 - 1, 512 - 1, 1024 - 1))
}
