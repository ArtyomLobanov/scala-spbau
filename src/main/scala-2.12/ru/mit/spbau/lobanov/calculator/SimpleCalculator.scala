package ru.mit.spbau.lobanov.calculator

import java.lang.Double.parseDouble
import java.util.Optional

object SimpleCalculator {

  private def doubleParser(token: String): Optional[Double] = {
    try {
      Optional.of(parseDouble(token))
    } catch {
      case e: Throwable => Optional.empty()
    }
  }

  val evaluator = new Evaluator[Double](doubleParser,
    new BinaryOperator[Double]("+", true, 1, a => b => a + b),
    new BinaryOperator[Double]("-", true, 1, a => b => a - b),
    new BinaryOperator[Double]("*", true, 2, a => b => a * b),
    new BinaryOperator[Double]("/", true, 2, a => b => a / b),
    new BinaryOperator[Double]("^", false, 3, a => b => math.pow(a, b)),
    new UnaryOperator[Double]("-", 1, x => -x),
    new Function[Double]("min", 2, args => math.min(args.head, args(1))),
    new Function[Double]("sqrt", 1, args => math.sqrt(args.head)),
    new Constant[Double]("Pi", math.Pi),
    new Constant[Double]("e", math.E))

  def main(args: Array[String]): Unit = {
    for (ln <- io.Source.stdin.getLines) {
      try {
        println(evaluator.evaluate(ln))
      } catch {
        case e: Exception =>
          println("Something went wrong! " + e.getMessage)
      }
    }
  }
}
