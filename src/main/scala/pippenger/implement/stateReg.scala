package pippenger.implement

import spinal.core._

class stateReg(G: Int, w: Int) extends Component {

  val io = new Bundle {
    val readAddress = Vec(in UInt (log2Up(G) + w bits), 2)
    val readData = Vec(out Bool(), 2)

    val writeAddress = Vec(in UInt (log2Up(G) + w bits), 2)
    val writeEn = Vec(in Bool(), 2)

    val softReset = in Bool()
  }

  val regs = Vec((Reg(Bits((1 << w) - 1 bits)) init (0)), G)

  val readArea = new Area {
    for (i <- 0 until 2) {
      io.readData(i) := RegNext(RegNext(RegNext(Vec(regs.map(_ ## False))(io.readAddress(i)(log2Up(G) + w - 1 downto w))(io.readAddress(i)(w - 1 downto 0)))))
    }
  }

  val writeArea = new Area {
    val flipAvailAble = Vec(Vec(Bits((1 << w) bits), G), 2)
    flipAvailAble.foreach(_.foreach(_.clearAll()))
    for (i <- 0 until 2) {
      when(io.writeEn(i)) {
        flipAvailAble(i)(io.writeAddress(i)(log2Up(G) + w - 1 downto w))(io.writeAddress(i)(w - 1 downto 0)) := True
      }
    }

    val regsNext = Vec(regs.zip(flipAvailAble.head.zip(flipAvailAble.last)).map { case (r, (f1, f2)) => r ^ f1((1 << w) - 1 downto 1) ^ f2((1 << w) - 1 downto 1) })
    //时序加强

    when(io.softReset) {
      regs.foreach(_.clearAll())
    }.otherwise {
      regs := regsNext
    }
  }

  def latency: Int = 3
}