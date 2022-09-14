import scala.collection.mutable.ArrayBuffer

val a = ArrayBuffer(1,2,3,4,5)
a --= a.take(3)
a
