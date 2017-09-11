package ru.mit.spbau.lobanov.calculator

class Context[T](elements: List[Element[T]]) {
  val functions: List[Function[T]] = elements
    .filter(element => element.isInstanceOf[Function[T]])
    .map(element => element.asInstanceOf[Function[T]])
  val constants: List[Constant[T]] = elements
    .filter(element => element.isInstanceOf[Constant[T]])
    .map(element => element.asInstanceOf[Constant[T]])
  val binaryOperators: List[BinaryOperator[T]] = elements
    .filter(element => element.isInstanceOf[BinaryOperator[T]])
    .map(element => element.asInstanceOf[BinaryOperator[T]])
  val unaryOperators: List[UnaryOperator[T]] = elements
    .filter(element => element.isInstanceOf[UnaryOperator[T]])
    .map(element => element.asInstanceOf[UnaryOperator[T]])
  val serviceSymbols: List[ServiceSymbol[T]] = elements
    .filter(element => element.isInstanceOf[ServiceSymbol[T]])
    .map(element => element.asInstanceOf[ServiceSymbol[T]])
}
