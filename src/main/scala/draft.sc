import scala.collection.mutable.ArrayBuffer
import scala.util.Random

val a = ArrayBuffer(1,2,3)
val segment = a.take(2)
a --= segment
a
segment