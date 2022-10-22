package org.datenlord
package examples

// example of implementing a simple parser using scala.util.parsing

import scala.util.parsing.combinator._
class SOPParsers extends RegexParsers {
  def expression: Parser[Any] = sum
  def sum: Parser[Any] = product ~ opt("+" ~ sum)
  def product: Parser[Any] = term ~ rep("*" ~ term)
  def term: Parser[Any] = number | "(" ~ expression ~ ")"
  def number: Parser[Any] = """0|[1-9][0-9]*(\.[0-9]+)?""".r
}

trait Expression {
  def execute: Double
}

case class Sum(val operand1: Expression, val operand2: Expression) extends Expression {
  def execute =
    if (operand2 == null) operand1.execute
    else operand1.execute + operand2.execute
}

case class Product(val operand1: Expression, val operands: List[Expression]) extends Expression {
  def execute =
    if (operands == Nil) operand1.execute
    else operand1.execute * operands.map(_.execute).product
}

case class Number(val value: Double) extends Expression {
  def execute = value
}

class SOP2Parsers extends RegexParsers {
  def expression: Parser[Expression] = sum
  def sum: Parser[Expression] = product ~ opt("+" ~ sum) ^^ {
    case p ~ None => p
    case p ~ Some("+" ~ s) => Sum(p, s)
  }
  def product: Parser[Expression] = term ~ rep("*" ~> term) ^^ {
    case t ~ Nil => t
    case t ~ terms => Product(t, terms)
  }
  def term: Parser[Expression] = number | "(" ~> expression <~ ")"
  def number: Parser[Number] = """0|[1-9][0-9]*(\.[0-9]+)?""".r ^^ (num => Number(num.toDouble))
}

object console {
  val parsers = new SOP2Parsers

  def execute(cmmd: String): String = {
    val result = parsers.parseAll(parsers.expression, cmmd)
    result match {
      case result: parsers.Failure => throw new Exception("syntax error")
      case _ => {
        val exp = result.get  // get the expression from the tree
        val value = exp.execute  // execute the expression
        value.toString  // return string representation of result
      }
    }
  }
  // etc.

  def main(args: Array[String]): Unit = {
    println(execute("3 + 5 * 2"))
  }
}