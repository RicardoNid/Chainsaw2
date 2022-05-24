package org.datenlord

import flowConverters.FlowConversion

import spinal.core._
import spinal.lib._
import flowConverters.ConversionMode._

import scala.reflect.ClassTag

case class TransformSystem(meshes: Seq[TransformMesh]) extends TransformConfig {

  def °(that: TransformMesh) = TransformSystem(that +: meshes)

  def fitTo(targetThroughput: Double) = {
    TransformSystem(meshes.map(_.fitTo(targetThroughput)))
  }

  override def impl(dataIn: Seq[Any]) = {
    var temp = dataIn
    meshes.foreach(mesh => temp = mesh.impl(temp))
    temp
  }

  def implStageByStage(dataIn: Seq[Any]) = {
    var temp = dataIn
    meshes.zipWithIndex.foreach { case (mesh, i) =>
      temp = mesh.impl(temp)
      println(s"stage ${i + 1}, ${temp.mkString(" ")}")
    }
    temp
  }

  override val size = (meshes.head.size._1, meshes.last.size._2)

  def conversions = meshes.prevAndNext { case (prev, next) => FlowConversion(prev.flowFormat, next.flowFormat) }

  override def latency = meshes.map(_.latency).sum + conversions.map(_.latency).sum

  override def flowFormat = MeshFormat.dontCare

  def period = meshes.map(_.flowFormat.period).max

  override def inputFlow = meshes.head.inputFlow.padTo(period)

  override def outputFlow = meshes.last.outputFlow.padTo(period)

  override def implH = {
    val systemConfig = this
    new TransformModule[Bits, Bits] {

      val modules = meshes.map(_.implH)
      val moduleIn = modules.head
      val moduleOut = modules.last

      val bitWidths = modules.prevAndNext { case (prev, next) =>
        assert(prev.outBitWidth == next.inBitWidth, s"${prev.outBitWidth},${next.inBitWidth}")
        prev.outBitWidth
      }

      val converters = conversions.zip(bitWidths).map { case (conversion, width) =>
        if (conversion.conversionMode == Pass) null
        else conversion.getConverterConfig(HardType(Bits(width bits))).implH
      }

      override val config = systemConfig
      override val dataIn = slave Flow Fragment(Vec(Bits(moduleIn.inBitWidth bits), moduleIn.inSize))
      override val dataOut = master Flow Fragment(Vec(Bits(moduleOut.outBitWidth bits), moduleOut.outSize))

      modules.init.zip(modules.tail).zip(converters).foreach { case ((prev, next), converter) =>
        if (converter == null) prev.dataOut >> next.dataIn
        else {
          val wrapper = converter.getWrapper()
          prev.dataOut >> wrapper._1
          wrapper._2 >> next.dataIn
        }
      }

      dataIn >> moduleIn.dataIn
      moduleOut.dataOut >> dataOut
    }
  }

  def implForTest[Tin <: Data : ClassTag, TOut <: Data : ClassTag](typeIn: HardType[Tin], typeOut: HardType[TOut]) = {
    val meshConfig = this
    new TransformModule[Tin, TOut] {
      override val config = meshConfig
      override val dataIn = slave Flow Fragment(Vec(typeIn, inputFlow.portWidth))
      override val dataOut = master Flow Fragment(Vec(typeOut, outputFlow.portWidth))

      val core: TransformModule[Bits, Bits] = implH
      core.dataIn.assignFromBits(dataIn.asBits)
      dataOut.assignFromBits(core.dataOut.asBits)
    }
  }
}
