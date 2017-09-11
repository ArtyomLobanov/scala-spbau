package ru.mit.spbau.lobanov.calculator

import org.junit.{Assert, Test}

class ReducerTest {

  private val uMinus = new UnaryOperator[Int]("-", 1, x => -x)
  private val uDollar = new UnaryOperator[Int]("$", 10, x => x * 30)
  private val bMinus = new BinaryOperator[Int]("-", true, 1, x => y => x - y)
  private val bPlus = new BinaryOperator[Int]("+", true, 1, x => y => x + y)
  private val bMult = new BinaryOperator[Int]("*", true, 2, x => y => x * y)
  private val bDivide = new BinaryOperator[Int]("/", true, 2, x => y => x / y)
  private val bPow = new BinaryOperator[Int]("^", false, 3, x => y => math.pow(x, y).toInt)
  private val thousand = new Constant[Int]("1000", 1000)
  private val ten = new Constant[Int]("10", 10)
  private val five = new Constant[Int]("5", 5)
  private val two = new Constant[Int]("2", 2)
  private val one = new Constant[Int]("1", 1)
  private val rnd = new Function[Int]("rnd", 0, _ => 7)
  private val sqr = new Function[Int]("sqr", 1, x => x.head * x.head)
  private val pow = new Function[Int]("pow", 2, x => math.pow(x.head, x(1)).toInt)

  private val evaluator = new Evaluator[Int](null, uMinus, uDollar, bMinus, bPlus, bMult, bDivide, bPow, rnd, sqr, pow)

  @Test
  def simpleArithmetic(): Unit = {
    val case1 = List(two, two, two, bMult, bPlus) // 2 + 2 * 2
    Assert.assertEquals(6, evaluator.reduce(case1))

    val case2 = List(two, two, bPlus, two, bMult) // (2 + 2) * 2
    Assert.assertEquals(8, evaluator.reduce(case2))

    val case3 = List(five, two, bPow, uMinus) // -5 ^ 2
    Assert.assertEquals(-25, evaluator.reduce(case3))

    val case6 = List(two, two, two, two, bPow, bPow, bPow) // 2 ^ 2 ^ 2 ^ 2
    Assert.assertEquals(65536, evaluator.reduce(case6))

    val case7 = List(two, five, uDollar, bMult, ten, bDivide) // 2 * $5 / 10
    Assert.assertEquals(30, evaluator.reduce(case7))

    val case8 = List(one, thousand, one, two, two, bMult, bPlus, two, bPow, bDivide, bPlus) // 1+1000 /(1+2*2) ^ 2
    Assert.assertEquals(41, evaluator.reduce(case8))
  }

  @Test
  def functions(): Unit = {
    val case2 = List(two, sqr, sqr) // sqr sqr 2
    Assert.assertEquals(16, evaluator.reduce(case2))

    val case3 = List(rnd, sqr, sqr) // sqrt sqrt rnd()
    Assert.assertEquals(7 * 7 * 7 * 7, evaluator.reduce(case3))

    val case4 = List(ten, two, pow) // pow (10, 2)
    Assert.assertEquals(100, evaluator.reduce(case4))

    val case5 = List(five, two, uMinus, bPlus, two, five, bMult, pow, uMinus) // -pow(5+(-2), 2*five)
    Assert.assertEquals(-math.pow(3, 10).toInt, evaluator.reduce(case5))
  }
}
