package ru.mit.spbau.lobanov.calculator

trait Element[T] {
  def getNotation: String

  def isTokenExpected(tokenType: TokenType): Boolean
}

trait Operator[T] extends Element[T] {
  def getPriority: Int
}

case class Function[T](notation: String, arity: Int, implementation: List[T] => T) extends Element[T] {
  override def getNotation: String = notation

  override def isTokenExpected(tokenType: TokenType): Boolean = tokenType == ServiceSymbolToken ||
    arity == 1 && (tokenType == ValueToken || tokenType == FunctionToken)

  def getArity: Int = arity

  def apply(array: List[T]): T = {
    if (array.length != arity) {
      throw new RuntimeException("Wrong number of arguments! Expected: " + array + ", found: " + array.length)
    }
    implementation.apply(array)
  }
}

case class ServiceSymbol[T](notation: String, expectedTokens: TokenType*) extends Element[T] {
  override def getNotation: String = notation

  override def isTokenExpected(tokenType: TokenType): Boolean = expectedTokens.contains(tokenType)
}

case class Constant[T](notation: String, value: T) extends Element[T] {
  def this(value: T) = this(value.toString, value)

  override def getNotation: String = notation

  override def isTokenExpected(tokenType: TokenType): Boolean =
    tokenType == BinaryOperatorToken || tokenType == ServiceSymbolToken || tokenType == Nothing

  def getValue: T = value
}

case class BinaryOperator[T](notation: String, leftAssociative: Boolean,
                             priority: Int,
                             implementation: T => T => T) extends Operator[T] {
  override def getNotation: String = notation

  def apply(left: T, right: T): T = implementation.apply(left).apply(right)

  def isLeftAssociative: Boolean = leftAssociative

  override def getPriority: Int = priority

  override def isTokenExpected(tokenType: TokenType): Boolean =
    tokenType == ValueToken || tokenType == ServiceSymbolToken || tokenType == FunctionToken  ||
      tokenType == UnaryOperatorToken

}

case class UnaryOperator[T](notation: String,
                            priority: Int,
                            implementation: T => T) extends Operator[T] {
  override def getNotation: String = notation

  def apply(value: T): T = implementation.apply(value)

  override def getPriority: Int = priority

  override def isTokenExpected(tokenType: TokenType): Boolean =
    tokenType == ValueToken || tokenType == ServiceSymbolToken || tokenType == FunctionToken
}














