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
  private val zero = new Constant[Int]("0", 0)
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

//  @Test
//  def functions(): Unit = {
//    val case1 = List(rnd, lBracket, rBracket, bPlus, Y, bMult, A) // rnd() + Y * A
//    val expectedResult1 = List(rnd, Y, A, bMult, bPlus)
//    Assert.assertEquals(expectedResult1, evaluator.toPostfixNotation(case1))
//
//    val case2 = List(sqrt, sqrt, X) // sqrt sqrt X
//    val expectedResult2 = List(X, sqrt, sqrt)
//    Assert.assertEquals(expectedResult2, evaluator.toPostfixNotation(case2))
//
//    val case3 = List(sqrt, sqrt, rnd, lBracket, rBracket) // sqrt sqrt rnd()
//    val expectedResult3 = List(rnd, sqrt, sqrt)
//    Assert.assertEquals(expectedResult3, evaluator.toPostfixNotation(case3))
//
//    val case4 = List(pow, lBracket, sqrt, lBracket, X, rBracket, comma, sqrt, Y, rBracket) // pow (sqrt(X), sqrt Y)
//    val expectedResult4 = List(X, sqrt, Y, sqrt, pow)
//    Assert.assertEquals(expectedResult4, evaluator.toPostfixNotation(case4))
//
//    val case5 = List(uMinus, pow, lBracket, X, bPlus, lBracket, uMinus, X, rBracket, comma, A, bMult, B, rBracket) // -pow(X+(-X), A*B)
//    val expectedResult5 = List(X, X, uMinus, bPlus, A, B, bMult, pow, uMinus)
//    Assert.assertEquals(expectedResult5, evaluator.toPostfixNotation(case5))
//  }
//
//  @Test
//  def highPriorityUnaryOperation(): Unit = {
//    val case1 = List(B, bPlus, Y, bMult, uDollar, A) // B + Y * $A
//    val expectedResult1 = List(B, Y, A, uDollar, bMult, bPlus)
//    Assert.assertEquals(expectedResult1, evaluator.toPostfixNotation(case1))
//
//    val case2 = List(uMinus, X, bPow, X, bPlus, uDollar, Y, bPow, uDollar, Y) // -X ^ X + $Y^$Y
//    val expectedResult2 = List(X, X, bPow, uMinus, Y, uDollar, Y, uDollar, bPow, bPlus)
//    Assert.assertEquals(expectedResult2, evaluator.toPostfixNotation(case2))
//  }
}
