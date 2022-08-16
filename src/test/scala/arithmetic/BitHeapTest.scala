package org.datenlord
package arithmetic

import dfg.ArithInfo

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

class BitHeapTest extends AnyFlatSpec {

  behavior of "bit heap"

  val smallHeap = BitHeap(ArrayBuffer(1, 3, 5, 4, 2).map(i => ArrayBuffer.fill(i)(1)), 0)

  it should "visualize itself correctly" in {
    println(smallHeap)
    println(Compressor4to2.toString(4))
  }

  case class BitHeapCompressor() extends Component {
    val dataIn = in Vec(UInt(40 bits), 20)
    val heap = BitHeap.fromOperands(dataIn.map(_.asBools), Seq.fill(50)(ArithInfo(40, 0, true)))

    def zero() = False

    def pipeline(bool: Bool) = bool.d(1)

    val ret = heap.compressAll(Seq(Compressor1to1, Compressor3to1, Compressor4to2), pipeline).output(zero)
    ret.map(_.asBits().asUInt).foreach(out(_))
  }

  it should "work correctly" in {
    VivadoSynth(BitHeapCompressor(), "bitHeap")
  }

}
