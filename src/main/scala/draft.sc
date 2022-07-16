import scala.collection.mutable.ArrayBuffer

val buffer = ArrayBuffer[Int]()

(0 until 10).foreach(i => buffer += i)
val slice = buffer.take(3)
buffer --= slice
buffer
buffer.take(20)

buffer ++= buffer