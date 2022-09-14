package pippenger

import spinal.core._

class regs(W: Int, w: Int) extends Component {

  val G = Math.ceil(W.toDouble / w).toInt

  val io = new Bundle {
    val readAddress = Vec(in UInt (log2Up(G) + w bits), 2)
    val readEn = Vec(in Bool(), 2)
    val readData = Vec(out Bool(), 2)

    val softReset = in Bool()
  }

  val regs = Vec((Reg(Bits((1 << w) - 1 bits)) init (0)), G)

  val readArea = new Area {
    for (i <- 0 until 2) {
      io.readData(i) := Vec(regs.map(_ ## Bool().assignDontCare()))(io.readAddress(i)(log2Up(G) + w - 1 downto w))(io.readAddress(i)(w - 1 downto 0))
    }
  }

  val writeArea = new Area {
    val flipAvailAble = Vec(Vec(Bits((1 << w) - 1 bits), G), 2)
    flipAvailAble.head.foreach(_.clearAll())
    flipAvailAble.last := flipAvailAble.head
    for (i <- 0 until 2) {
      when(io.readEn(i)) {
        flipAvailAble(i)(io.readAddress(i)(log2Up(G) + w - 1 downto w))(io.readAddress(i)(w - 1 downto 0)) := True
      }
    }
    val regsNext = Vec(regs.zip(flipAvailAble.last).map { case (r, f) => r ^ f })

    when(io.softReset) {
      regs.foreach(_.clearAll())
    }.otherwise {
      regs := regsNext
    }
  }
}

import org.datenlord._

object regs extends App {
  VivadoImpl(new regs(253, 10))
}