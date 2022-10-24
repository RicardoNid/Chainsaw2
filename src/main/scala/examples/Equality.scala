package examples

case class Equality(a:Int, b:Int) {
  override def equals(that: Any) = {
    this.a == that.asInstanceOf[Equality].a &&
      this.b == that.asInstanceOf[Equality].b
  }
  override def hashCode() = 1
}

object Equality extends App {
  println(s"Equality(1,2) == Equality(2,3) = ${Equality(1,2) == Equality(2,3)}")
  println(s"Equality(1,1) == Equality(1,1) = ${Equality(1,1) == Equality(1,1)}")
}


class EqualityAnother(val a:Int, val b:Int) {
  override def equals(that: Any) = {
    this.a == that.asInstanceOf[EqualityAnother].a &&
      this.b == that.asInstanceOf[EqualityAnother].b
  }
}

object EqualityAnother extends App {
  val a = new EqualityAnother(1,1)
  val b = new EqualityAnother(1,1)
  println(s"a.hashCode() = ${a.hashCode()}")
  println(s"b.hashCode() = ${b.hashCode()}")
  println(s"EqualityAnother(1,2) == EqualityAnother(2,3) = ${new EqualityAnother(1,2) == new EqualityAnother(2,3)}")
  println(s"EqualityAnother(1,1) == EqualityAnother(1,1) = ${a == b}")
}