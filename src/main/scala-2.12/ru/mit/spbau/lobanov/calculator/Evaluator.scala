package ru.mit.spbau.lobanov.calculator

import java.util.Optional

import scala.collection.mutable

class Evaluator[T](valueParser: String => Optional[T], elements: Element[T]*) {
  val leftBracket: ServiceSymbol[T] =
    new ServiceSymbol[T]("(", ValueToken, FunctionToken, ServiceSymbolToken, UnaryOperatorToken)
  val rightBracket: ServiceSymbol[T] = new ServiceSymbol[T](")", BinaryOperatorToken, ServiceSymbolToken, Nothing)
  val separator: ServiceSymbol[T] =
    new ServiceSymbol[T](",", ValueToken, FunctionToken, ServiceSymbolToken, UnaryOperatorToken)

  private val context = new Context[T](elements.toList ++ List(leftBracket, rightBracket, separator))
  private val parser = new Parser[T](context, valueParser)

  def evaluate(input: String): T = {
    val elements = parser.parse(input)
    val toEvaluate = toPostfixNotation(elements.head)
    reduce(toEvaluate)
  }

  def reduce(postfixNotation: List[Element[T]]): T = {
    val stack = new java.util.LinkedList[T]()
    for (element <- postfixNotation) {
      element match {
        case Constant(_, value) => stack.push(value)
        case UnaryOperator(_, _, implementation) => stack.push(implementation.apply(stack.poll()))
        case BinaryOperator(_, _, _, _) =>
          val secondArgument = stack.poll()
          val firstArgument = stack.poll()
          stack.push(element.asInstanceOf[BinaryOperator[T]]
            .apply(firstArgument, secondArgument))
        case Function(_, arity, implementation) =>
          val arguments = new mutable.MutableList[T]()
          for (_ <- 0 until arity)
            arguments += stack.poll()
          stack.push(implementation.apply(arguments.reverse.toList))
        case _ => println("Unexpected situation")
      }
    }
    stack.poll()
  }

  def toPostfixNotation(elements: List[Element[T]]): List[Element[T]] = {
    val stack = new java.util.LinkedList[Element[T]]()
    val result = new mutable.MutableList[Element[T]]()
    for (element <- elements) {
      element match {
        case Constant(_, _) => result += element
        case Function(_, _, _) => stack.push(element)
        case ServiceSymbol(_, _*) => processServiceSymbol(element.asInstanceOf[ServiceSymbol[T]], stack, result)
        case UnaryOperator(_, _, _) =>
          processUnaryOperator(element.asInstanceOf[UnaryOperator[T]], stack, result)
        case BinaryOperator(_, _, _, _) =>
          processBinaryOperator(element.asInstanceOf[BinaryOperator[T]], stack, result)
        case _ => println("Unexpected situation")
      }
    }
    while (!stack.isEmpty)
      result += stack.poll()
    result.toList
  }

  private def processServiceSymbol(symbol: ServiceSymbol[T],
                                   stack: java.util.LinkedList[Element[T]],
                                   result: mutable.MutableList[Element[T]]): Unit = {
    if (symbol == leftBracket) {
      stack.push(symbol)
    } else if (symbol == rightBracket) {
      while (!stack.isEmpty && stack.peek() != leftBracket) {
        result += stack.poll()
      }
      stack.poll()
      while (!stack.isEmpty && stack.peek().isInstanceOf[Function[T]]) {
        result += stack.poll()
      }
    } else if (symbol == separator) {
      while (!stack.isEmpty && stack.peek() != leftBracket) {
        result += stack.poll()
      }
    }
  }

  private def processBinaryOperator(operator: BinaryOperator[T],
                                    stack: java.util.LinkedList[Element[T]],
                                    result: mutable.MutableList[Element[T]]): Unit = {
    while (!stack.isEmpty && (stack.peek().isInstanceOf[Function[T]] || stack.peek().isInstanceOf[Operator[T]]
      && isHigherPriority(operator, stack.peek().asInstanceOf[Operator[T]]))) {
      result += stack.poll()
    }
    stack.push(operator)
  }

  private def processUnaryOperator(operator: UnaryOperator[T],
                                   stack: java.util.LinkedList[Element[T]],
                                   result: mutable.MutableList[Element[T]]): Unit = {
    if (!stack.isEmpty && !(stack.peek() == leftBracket)) {
      val head = stack.peek()
      if (!(head.isInstanceOf[Operator[T]] && operator.priority > head.asInstanceOf[Operator[T]].getPriority))
        throw new RuntimeException("Syntax error")
    }
    stack.push(operator)
  }

  private def isHigherPriority(binaryOperator: BinaryOperator[T], another: Operator[T]): Boolean = {
    binaryOperator.getPriority == another.getPriority && binaryOperator.isLeftAssociative ||
      binaryOperator.getPriority < another.getPriority
  }
}
