package ru.mit.spbau.lobanov.calculator

import java.lang.Integer.parseInt
import java.util.Optional

import org.junit.{Assert, Test}

class CalculatorTest {
  private def intParser(token: String): Optional[Int] = {
    try {
      Optional.of(parseInt(token))
    } catch {
      case e: Throwable => Optional.empty()
    }
  }

  private def boolParser(token: String): Optional[Boolean] = {
    try {
      parseInt(token) match {
        case 0 => Optional.of(false)
        case 1 => Optional.of(true)
        case _ => Optional.empty()
      }
    } catch {
      case e: Throwable => Optional.empty()
    }
  }

  val arithmeticEvaluator = new Evaluator[Int](intParser,
    new BinaryOperator[Int]("+", true, 1, a => b => a + b),
    new BinaryOperator[Int]("-", true, 1, a => b => a - b),
    new BinaryOperator[Int]("*", true, 2, a => b => a * b),
    new BinaryOperator[Int]("/", true, 2, a => b => a / b),
    new BinaryOperator[Int]("^", false, 3, a => b => math.pow(a, b).toInt),
    new UnaryOperator[Int]("-", 1, x => -x),
    new Function[Int]("minus", 2, args => args.head - args(1)),
    new Function[Int]("sqr", 1, args => args.head * args.head),
    new Constant[Int]("Pi", 3),
    new Function[Int]("zero", 0, _ => 0))

  val logicEvaluator = new Evaluator[Boolean](boolParser,
    new BinaryOperator[Boolean]("||", true, 1, a => b => a || b),
    new BinaryOperator[Boolean]("&&", true, 2, a => b => a && b),
    new UnaryOperator[Boolean]("!", 3, a => !a),
    new Constant[Boolean]("T", true),
    new Constant[Boolean]("F", false),
    new Function[Boolean]("xor", 2, args => args.head ^ args(1)))

  @Test
  def arithmeticTest(): Unit = {
    val case1 = "2*5-4"
    Assert.assertEquals(6, arithmeticEvaluator.evaluate(case1))

    val case2 = "2*(5-7)"
    Assert.assertEquals(-4, arithmeticEvaluator.evaluate(case2))

    val case3 = "15+21/3*5"
    Assert.assertEquals(50, arithmeticEvaluator.evaluate(case3))

    val case4 = "15+36/sqr3"
    Assert.assertEquals(19, arithmeticEvaluator.evaluate(case4))

    val case5 = "2^2^2^2"
    Assert.assertEquals(65536, arithmeticEvaluator.evaluate(case5))

    val case6 = "-2^2"
    Assert.assertEquals(-4, arithmeticEvaluator.evaluate(case6))

    val case7 = "-Pi-2"
    Assert.assertEquals(-5, arithmeticEvaluator.evaluate(case7))

    val case8 = "-7-(-2)"
    Assert.assertEquals(-5, arithmeticEvaluator.evaluate(case8))
  }

  @Test
  def logicTest(): Unit = {
    val case1 = "0 && 1"
    Assert.assertEquals(false, logicEvaluator.evaluate(case1))

    val case2 = "T || 1 && F"
    Assert.assertEquals(true, logicEvaluator.evaluate(case2))

    val case3 = "!T || !F"
    Assert.assertEquals(true, logicEvaluator.evaluate(case3))

    val case4 = "xor(!F, T && !F)"
    Assert.assertEquals(false, logicEvaluator.evaluate(case4))
  }

  @Test
  def functionsTest(): Unit = {
    val case1 = "sqr sqr 2"
    Assert.assertEquals(16, arithmeticEvaluator.evaluate(case1))

    val case2 = "minus(6, 3)"
    Assert.assertEquals(3, arithmeticEvaluator.evaluate(case2))

    val case3 = "Pi * zero()"
    Assert.assertEquals(0, arithmeticEvaluator.evaluate(case3))

    val case4 = "sqr(minus(-2, 1))"
    Assert.assertEquals(9, arithmeticEvaluator.evaluate(case4))
  }
}
