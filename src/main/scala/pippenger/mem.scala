package pippenger

import spinal.core._
import spinal.lib._

case class memPort[T <: Data](dataType: HardType[T], addressWidth: Int) extends Bundle with IMasterSlave {
  val address = UInt(addressWidth bits)
  val writeData = dataType()
  val readData = dataType()
  val we = Bool()
  val ce = Bool()

  override def asMaster(): Unit = {
    out(address, writeData, we, ce)
    in(readData)
  }
}

class mem[T <: Data](dataType: HardType[T], W: Int, w: Int) extends Component {
  val G = Math.ceil(W.toDouble / w).toInt

  val io = new Bundle {
    val port = Vec(slave(memPort(dataType, log2Up(G) + w)), 2)
  }

  val mem = Mem(dataType(), G << w)

  for (i <- 0 until 2) {
    io.port(i).readData := mem.readWriteSync(io.port(i).address, io.port(i).writeData, io.port(i).we, io.port(i).ce)
  }
}

import org.datenlord._

object mem extends App {
  VivadoImpl(new mem(UInt(1131 bits), 253, 12))
}