package ru.mit.spbau.lobanov.calculator

import java.lang.Integer.parseInt
import java.util.Optional

import org.junit.{Assert, Test}

class ParserTest {

  private def intParser(token: String): Optional[Int] = {
    try {
      Optional.of(parseInt(token))
    } catch {
      case e: Throwable => Optional.empty()
    }
  }

  private val unaryMinus = new UnaryOperator[Int]("-", 1, x => -x)
  private val binaryMinus = new BinaryOperator[Int]("-", true, 1, x => y => x - y)
  private val binaryPlus = new BinaryOperator[Int]("+", true, 1, x => y => x + y)
  private val leftBracket =
    new ServiceSymbol[Int]("(", ValueToken, FunctionToken, ServiceSymbolToken, UnaryOperatorToken)
  private val rightBracket = new ServiceSymbol[Int](")", BinaryOperatorToken, ServiceSymbolToken, Nothing)
  private val separator = new ServiceSymbol[Int](",", ValueToken, FunctionToken, ServiceSymbolToken, UnaryOperatorToken)

  @Test
  def differentMinusesTest(): Unit = {
    val maxFunction = new Function[Int]("max", 2, args => math.max(args.head, args(1)))
    val context = new Context[Int](List(unaryMinus, binaryMinus, maxFunction, leftBracket, rightBracket, separator))
    val parser = new Parser[Int](context, intParser)

    val case1 = parser.parse("-2")
    Assert.assertEquals(1, case1.size)
    Assert.assertSame(unaryMinus, case1.head.head)

    val case2 = parser.parse("2-2")
    Assert.assertEquals(1, case2.size)
    Assert.assertSame(binaryMinus, case2.head(1))

    val case3 = parser.parse("-2-3")
    Assert.assertEquals(1, case3.size)
    Assert.assertSame(unaryMinus, case3.head.head)
    Assert.assertSame(binaryMinus, case3.head(2))

    val case4 = parser.parse("-2-(-3)")
    Assert.assertEquals(1, case4.size)
    Assert.assertSame(unaryMinus, case4.head.head)
    Assert.assertSame(binaryMinus, case4.head(2))
    Assert.assertSame(unaryMinus, case4.head(4))

    val case5 = parser.parse("-2-max(-3, -2)")
    Assert.assertEquals(1, case5.size)
    Assert.assertSame(unaryMinus, case5.head.head)
    Assert.assertSame(binaryMinus, case5.head(2))
    Assert.assertSame(unaryMinus, case5.head(5))
    Assert.assertSame(unaryMinus, case5.head(8))
  }

  @Test
  def variantReading(): Unit = {
    val increment = new UnaryOperator[Int]("++", 5, x => x + 1)
    val context1 = new Context[Int](List(increment, binaryPlus))
    val parser1 = new Parser[Int](context1, intParser)

    val case1 = parser1.parse("2+2")
    Assert.assertEquals(1, case1.size)
    Assert.assertSame(binaryPlus, case1.head(1))

    val case2 = parser1.parse("++2")
    Assert.assertEquals(1, case2.size)
    Assert.assertSame(increment, case2.head.head)

    val case3 = parser1.parse("2+++2")
    Assert.assertEquals(1, case3.size)
    Assert.assertSame(binaryPlus, case3.head(1))
    Assert.assertSame(increment, case3.head(2))

    val incrementAndSum = new BinaryOperator[Int]("+++", true, 1, x => y => y + x + 1)
    val context2 = new Context[Int](List(increment, binaryPlus, incrementAndSum))
    val parser2 = new Parser[Int](context2, intParser)

    val case4 = parser2.parse("2+++2")
    Assert.assertEquals(2, case4.size)

    val case5 = parser2.parse("2+++++2") // = 2 +++ (++2)
    Assert.assertEquals(1, case5.size)
  }

  @Test
  def functionParsing(): Unit = {
    val max = new Function[Int]("max", 2, args => math.max(args.head, args(1)))
    val sqrt = new Function[Int]("sqrt", 1, args => math.sqrt(args.head).toInt)
    val rnd = new Function[Int]("rnd", 0, args => (math.random() * 10).toInt)
    val context = new Context[Int](List(max, sqrt, rnd, leftBracket, rightBracket, separator))
    val parser = new Parser[Int](context, intParser)
    Assert.assertEquals(0, parser.parse("max").size)
    Assert.assertEquals(0, parser.parse("sqrt").size)
    Assert.assertEquals(0, parser.parse("rnd").size)
    Assert.assertEquals(1, parser.parse("rnd()").size)
    Assert.assertEquals(0, parser.parse("sqrt sqrt").size)
    Assert.assertEquals(1, parser.parse("sqrt sqrt 4").size)
    Assert.assertEquals(1, parser.parse("sqrt rnd()").size)
    Assert.assertEquals(0, parser.parse("max 2 4").size)
    Assert.assertEquals(1, parser.parse("max (2, 4)").size)
    Assert.assertEquals(1, parser.parse("max(2,4)").size)
    Assert.assertEquals(1, parser.parse("sqrtsqrtrnd()").size)
  }

  @Test
  def constantValues(): Unit = {
    val sqrt = new Function[Int]("sqrt", 1, args => math.sqrt(args.head).toInt)
    val ten = new Constant[Int]("ten", 10)
    val context = new Context[Int](List(sqrt, ten, binaryMinus, unaryMinus, leftBracket, rightBracket, separator))
    val parser = new Parser[Int](context, intParser)
    Assert.assertEquals(1, parser.parse("sqrt10").size)
    Assert.assertEquals(1, parser.parse("sqrtten").size)
    Assert.assertEquals(1, parser.parse("sqrt ten").size)
    Assert.assertEquals(1, parser.parse("sqrt(ten)").size)
    Assert.assertEquals(0, parser.parse("sqrt(te").size)
    Assert.assertEquals(0, parser.parse("sqrt(te)n").size)
    Assert.assertEquals(0, parser.parse("ten-(ten)").size)
  }
}
