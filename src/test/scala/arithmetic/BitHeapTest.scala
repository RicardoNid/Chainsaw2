package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class BitHeapTest extends AnyFlatSpec {

  behavior of "bit heap"

  val smallHeap =
    BitHeap(ArrayBuffer(1, 3, 5, 4, 2).map(ArrayBuffer.fill(_)(0)), 0, 0)

  it should "visualize itself correctly" in {
    println(smallHeap)
    println(Compressor4to2.toString(4))
  }

  case class BitHeapCompressor() extends Component {
    val dataIn = in Vec (UInt(40 bits), 20)
    val heap = BitHeap.getHeapFromInfos(
      Seq(Seq.fill(50)(ArithInfo(40, 0, true))),
      Seq(dataIn.map(_.asBools))
    )

    def zero(): Bool = False

    def pipeline(bool: Bool) = bool.d(1)

    val (ret, solutions) = heap.compressAll(Gpcs())
    ret.output(zero).map(_.asBits().asUInt).foreach(out(_))
  }

  it should "work correctly on hardware" in VivadoSynth(
    BitHeapCompressor(),
    "bitHeap"
  )

}
