import scala.collection.mutable.ArrayBuffer

var a = 3

val list = ArrayBuffer[Int]()

list += a

a = 4

list += a

list

a = 5

list
