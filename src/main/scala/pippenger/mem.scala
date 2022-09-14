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

  val mem = Mem(dataType(), G << w) addAttribute("ram_style", "ultra")

  io.port(0).readData := mem.readWriteSync(io.port(0).address, io.port(0).writeData, io.port(0).we, io.port(0).ce, duringWrite = dontRead)
  io.port(1).readData := mem.readWriteSync(io.port(1).address, io.port(1).writeData, io.port(1).we, io.port(1).ce, duringWrite = dontRead)
}

import org.datenlord._

object mem extends App {
  VivadoImpl(new mem(UInt(1131 bits), 253, 10))
}