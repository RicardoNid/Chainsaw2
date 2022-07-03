import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import collection.mutable

val m = mutable.Map[Int, Int]()

Seq(1,2,3).scan(0)(_ + _)

Seq.iterate(0, 3)

