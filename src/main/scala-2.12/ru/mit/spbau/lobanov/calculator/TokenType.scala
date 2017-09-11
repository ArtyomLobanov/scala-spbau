package ru.mit.spbau.lobanov.calculator

trait TokenType

case object FunctionToken extends TokenType

case object BinaryOperatorToken extends TokenType

case object UnaryOperatorToken extends TokenType

case object ValueToken extends TokenType

case object ServiceSymbolToken extends TokenType

case object Nothing extends TokenType