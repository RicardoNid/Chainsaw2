package org.datenlord
package examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object UseSameComponent {

  case class UniqueModuleConfig(width: Int) {


    def solve() = {
      println("run solve")
    }

    def implH() = {
      //
      //      def gen() = {
      //        val module = UniqueModule(width)
      //        moduleList(hashCode()) = module
      //        module
      //      }
      //
      //      moduleList.getOrElse(hashCode(), gen()).asInstanceOf[UniqueModule]
      println("executed")
      // val module =
      new Component {
        setDefinitionName("add")
        val a, b = in UInt (width bits)
        val z = out UInt (width + 1 bits)
        z := a +^ b
      }

      //      UniqueModule(this)
    }
  }

  case class UniqueModule(config: UniqueModuleConfig) extends Component {

    import config._

    val a, b = in UInt (width bits)
    val z = out UInt (width + 1 bits)
    z := a +^ b
  }

  def main(args: Array[String]): Unit = {
    RtlGen {
      new Module {
        val a, b = in UInt (6 bits)
        val c, d = in UInt (7 bits)
        val y = out UInt (7 bits)
        val z = out UInt (8 bits)

        val core0 = UniqueModuleConfig(6).implH()
        val core1 = UniqueModuleConfig(7).implH()

        core0.a := a
        core0.b := b
        core1.a := c
        core1.b := d

        y := core0.z
        z := core1.z

      }
    }
  }
}