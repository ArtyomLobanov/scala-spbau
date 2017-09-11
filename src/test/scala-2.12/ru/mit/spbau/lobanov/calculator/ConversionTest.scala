package ru.mit.spbau.lobanov.calculator

import org.junit.{Assert, Test}

class ConversionTest {

  private val uMinus = new UnaryOperator[Int]("-", 1, x => -x)
  private val uDollar = new UnaryOperator[Int]("$", 10, x => x * 30)
  private val bMinus = new BinaryOperator[Int]("-", true, 1, x => y => x - y)
  private val bPlus = new BinaryOperator[Int]("+", true, 1, x => y => x + y)
  private val bMult = new BinaryOperator[Int]("*", true, 2, x => y => x * y)
  private val bDivide = new BinaryOperator[Int]("/", true, 2, x => y => x / y)
  private val bPow = new BinaryOperator[Int]("^", false, 3, x => y => math.pow(x, y).toInt)
  private val X = new Constant[Int]("x", 0)
  private val Y = new Constant[Int]("y", 0)
  private val A = new Constant[Int]("a", 0)
  private val B = new Constant[Int]("b", 0)
  private val rnd = new Function[Int]("rnd", 0, _ => 239566)
  private val sqrt = new Function[Int]("sqrt", 1, x => math.sqrt(x.head).toInt)
  private val pow = new Function[Int]("pow", 2, x => math.pow(x.head, x(1)).toInt)

  private val evaluator = new Evaluator[Int](null, uMinus, uDollar, bMinus, bPlus, bMult, bDivide, bPow, X, Y, A, B,
    rnd, sqrt, pow)

  private val lBracket = evaluator.leftBracket
  private val rBracket = evaluator.rightBracket
  private val comma = evaluator.separator

  @Test
  def simpleArithmetic(): Unit = {
    val case1 = List(X, bPlus, Y, bMult, A) // X + Y * A
    val expectedResult1 = List(X, Y, A, bMult, bPlus)
    Assert.assertEquals(expectedResult1, evaluator.toPostfixNotation(case1))

    val case2 = List(lBracket, X, bPlus, Y, rBracket, bMult, A) // (X + Y) * A
    val expectedResult2 = List(X, Y, bPlus, A, bMult)
    Assert.assertEquals(expectedResult2, evaluator.toPostfixNotation(case2))

    val case3 = List(uMinus, X, bPow, A) // -X ^ A
    val expectedResult3 = List(X, A, bPow, uMinus)
    Assert.assertEquals(expectedResult3, evaluator.toPostfixNotation(case3))

    val case4 = List(uMinus, lBracket, X, rBracket, bPow, A) // -(X) ^ A
    val expectedResult4 = List(X, A, bPow, uMinus)
    Assert.assertEquals(expectedResult4, evaluator.toPostfixNotation(case4))

    val case5 = List(lBracket, uMinus, X, rBracket, bPow, A) // (-X) ^ A
    val expectedResult5 = List(X, uMinus, A, bPow)
    Assert.assertEquals(expectedResult5, evaluator.toPostfixNotation(case5))

    val case6 = List(X, bPow, Y, bPow, A) // X ^ Y ^ A
    val expectedResult6 = List(X, Y, A, bPow, bPow)
    Assert.assertEquals(expectedResult6, evaluator.toPostfixNotation(case6))

    val case7 = List(X, bPlus, Y, bPow, A) // X + Y ^ A
    val expectedResult7 = List(X, Y, A, bPow, bPlus)
    Assert.assertEquals(expectedResult7, evaluator.toPostfixNotation(case7))

    val case8 = List(X, bPlus, Y, bDivide, lBracket, A, bPlus, B, bMult, B, rBracket, bPow, X) // X+Y /(A+B*B) ^ X
    val expectedResult8 = List(X, Y, A, B, B, bMult, bPlus, X, bPow, bDivide, bPlus)
    Assert.assertEquals(expectedResult8, evaluator.toPostfixNotation(case8))
  }

  @Test
  def functions(): Unit = {
    val case1 = List(rnd, lBracket, rBracket, bPlus, Y, bMult, A) // rnd() + Y * A
    val expectedResult1 = List(rnd, Y, A, bMult, bPlus)
    Assert.assertEquals(expectedResult1, evaluator.toPostfixNotation(case1))

    val case2 = List(sqrt, sqrt, X) // sqrt sqrt X
    val expectedResult2 = List(X, sqrt, sqrt)
    Assert.assertEquals(expectedResult2, evaluator.toPostfixNotation(case2))

    val case3 = List(sqrt, sqrt, rnd, lBracket, rBracket) // sqrt sqrt rnd()
    val expectedResult3 = List(rnd, sqrt, sqrt)
    Assert.assertEquals(expectedResult3, evaluator.toPostfixNotation(case3))

    val case4 = List(pow, lBracket, sqrt, lBracket, X, rBracket, comma, sqrt, Y, rBracket) // pow (sqrt(X), sqrt Y)
    val expectedResult4 = List(X, sqrt, Y, sqrt, pow)
    Assert.assertEquals(expectedResult4, evaluator.toPostfixNotation(case4))

    val case5 = List(uMinus, pow, lBracket, X, bPlus, lBracket, uMinus, X, rBracket, comma, A, bMult, B, rBracket) // -pow(X+(-X), A*B)
    val expectedResult5 = List(X, X, uMinus, bPlus, A, B, bMult, pow, uMinus)
    Assert.assertEquals(expectedResult5, evaluator.toPostfixNotation(case5))
  }

  @Test
  def highPriorityUnaryOperation(): Unit = {
    val case1 = List(B, bPlus, Y, bMult, uDollar, A) // B + Y * $A
    val expectedResult1 = List(B, Y, A, uDollar, bMult, bPlus)
    Assert.assertEquals(expectedResult1, evaluator.toPostfixNotation(case1))

    val case2 = List(uMinus, X, bPow, X, bPlus, uDollar, Y, bPow, uDollar, Y) // -X ^ X + $Y^$Y
    val expectedResult2 = List(X, X, bPow, uMinus, Y, uDollar, Y, uDollar, bPow, bPlus)
    Assert.assertEquals(expectedResult2, evaluator.toPostfixNotation(case2))
  }
}
