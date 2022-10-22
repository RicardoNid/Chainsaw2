package ip.pippenger

import spinal.core._
import spinal.lib._

class pippenger[T <: Data](
                            W: Int, w: Int, N: Int, pType: HardType[T],
                            pInit: => T, latency: Int, fifoDepth: Int
                          ) extends Component {
  require(w > 1)
  val G = Math.ceil(W.toDouble / w).toInt
  require(latency >= 2 * G + 1)

  val io = new Bundle {

    val inputData = slave Stream new Bundle {
      val k = UInt(W bits)
      val p = pType()
    }

    val adderPort = Vec(master(new Bundle with IMasterSlave {
      val a = pType()
      val b = pType()
      val s = pType()

      override def asMaster(): Unit = {
        out(a, b)
        in(s)
      }
    }), 2)

    val sum = master Flow pType()

  }

}