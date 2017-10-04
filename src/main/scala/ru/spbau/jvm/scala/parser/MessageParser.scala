package ru.spbau.jvm.scala.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class MessageParser extends RegexParsers {
  override val whiteSpace: Regex = "[ \t\r\f]+".r
  val seriesParser: Parser[String] = "\".*\"".r

  val intParser: Parser[Int] = "[0-9]+".r ^^ {
    _.toInt
  }

  val addSeries: Parser[AddSeries] =
    "новый сериал".r ~> seriesParser ^^ {
      name => AddSeries(name)
    }

  val deleteSeries: Parser[ForgetSeries] =
    "(удалить)|(забыть)".r ~> seriesParser ^^ (name => ForgetSeries(name))

  val defineEpisode: Parser[DefineEpisode] = "задать серию".r ~> seriesParser ~
    ("=".r ~> intParser <~ "s") ~ (intParser <~ "e".r) ^^ {
    case name ~ season ~ episode => DefineEpisode(name, season, episode)
  }

  val showSeries: Parser[ShowSeries.type] = "(список)|(мои сериалы)|(все сериалы)".r ^^ (_ => ShowSeries)

  val remindEpisode: Parser[RemindEpisode] = "какая серия в".r ~> seriesParser ^^ (
    name => RemindEpisode(name))

  val startSession: Parser[StartSession] = "начать сессию".r ~> seriesParser ^^ (
    name => StartSession(name))

  val stopSession: Parser[StopSession.type] = "закончить сессию".r ^^ (
    _ => StopSession)

  val nextEpisode: Parser[NextEpisode.type] = "следующая серия".r ^^ (_ => NextEpisode)

  val nextSeason: Parser[NextSeason.type] = "следующий сезон".r ^^ (_ => NextSeason)

  val userMessage: Parser[UserMessage] =
    addSeries | deleteSeries | defineEpisode | startSession | stopSession |
      nextEpisode | nextSeason | showSeries | remindEpisode

  override def skipWhitespace = true
}

object MessageParser extends MessageParser {
  def parse(text: String): UserMessage = {
    parseAll(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}
