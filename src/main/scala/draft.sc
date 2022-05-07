import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

val n = 4
val s = 2
val exps = Seq.tabulate(n - s, s)((i, j) => i - j + s - 1).flatten
exps.map(1 << _).inits.toSeq.map(_.sum).tail.reverse