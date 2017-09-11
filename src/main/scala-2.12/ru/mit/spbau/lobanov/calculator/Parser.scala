package ru.mit.spbau.lobanov.calculator

import java.util.Optional

import scala.collection.mutable

class Parser[T](context: Context[T], valueParser: String => Optional[T]) {

  private object BeginOfExpression extends Element[T] {
    override def getNotation: String = ""

    override def isTokenExpected(tokenType: TokenType): Boolean = tokenType == ValueToken ||
      tokenType == FunctionToken || tokenType == ServiceSymbolToken || tokenType == UnaryOperatorToken
  }

  def parse(input: String): List[List[Element[T]]] = {
    val initialState = new ParsingState(input, skipWhitespace(0, input), BeginOfExpression, null)
    val queue = mutable.Queue[ParsingState](initialState)
    val results = new mutable.MutableList[List[Element[T]]]()
    while (queue.nonEmpty) {
      val state = queue.dequeue()
      if (state.isTerminal) {
        if (state.isTokenExpected(Nothing))
          results += buildResult(state)
      } else {
        queue ++= parseElement(state)
      }
    }
    results.toList
  }

  private def parseElement(state: ParsingState): List[ParsingState] = {
    var results = new mutable.MutableList[ParsingState]()
    if (state.isTokenExpected(ValueToken)) {
      results ++= parseValue(state)
      results ++= parseCandidates(state, context.constants)
    }
    if (state.isTokenExpected(FunctionToken))
      results ++= parseCandidates(state, context.functions)
    if (state.isTokenExpected(ServiceSymbolToken))
      results ++= parseCandidates(state, context.serviceSymbols)
    if (state.isTokenExpected(UnaryOperatorToken))
      results ++= parseCandidates(state, context.unaryOperators)
    if (state.isTokenExpected(BinaryOperatorToken)) {
      results ++= parseCandidates(state, context.binaryOperators)
    }
    results.toList
  }

  private def parseValue(state: ParsingState): List[ParsingState] = {
    val input = state.input
    val results = new mutable.MutableList[ParsingState]
    var numberEnd = state.caret
    while (numberEnd < input.length && (input(numberEnd).isDigit || input(numberEnd) == '.')) {
      numberEnd += 1
      val token = input.substring(state.caret, numberEnd)
      val value = valueParser(token)
      if (value.isPresent) {
        results += new ParsingState(input, skipWhitespace(numberEnd, input), new Constant[T](value.get()), state)
      }
    }
    results.toList
  }

  private def parseCandidates(state: ParsingState, candidates: List[Element[T]]): List[ParsingState] = {
    var results = new mutable.MutableList[ParsingState]()
    for (candidate <- candidates) {
      if (state.input.startsWith(candidate.getNotation, state.caret)) {
        val nextTokenStart = skipWhitespace(state.caret + candidate.getNotation.length, state.input)
        results += new ParsingState(state.input, nextTokenStart, candidate, state)
      }
    }
    results.toList
  }

  private def skipWhitespace(caret: Int, input: String): Int = {
    var index = caret
    while (index < input.length && input(index).isWhitespace)
      index += 1
    index
  }

  private def buildResult(parsingState: ParsingState): List[Element[T]] = {
    val result = new mutable.MutableList[Element[T]]
    var currentState = parsingState
    while (currentState.lastParsed != BeginOfExpression) {
      result += currentState.lastParsed
      currentState = currentState.previousState
    }
    result.reverse.toList
  }

  private class ParsingState(val input: String,
                             val caret: Int,
                             val lastParsed: Element[T],
                             val previousState: ParsingState) {
    def isTerminal: Boolean = input.length == caret

    def isTokenExpected(tokenType: TokenType): Boolean = lastParsed.isTokenExpected(tokenType)
  }

}



