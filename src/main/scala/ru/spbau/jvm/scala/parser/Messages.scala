package ru.spbau.jvm.scala.parser

/**
  * @author Alefas
  */
trait UserMessage

case class AddSeries(series: String) extends UserMessage

case class ForgetSeries(series: String) extends UserMessage

case class DefineEpisode(series: String, season: Int, episode: Int) extends UserMessage

case class StartSession(series: String) extends UserMessage

case object StopSession extends UserMessage

case object NextEpisode extends UserMessage

case object NextSeason extends UserMessage

case class RemindEpisode(series: String) extends UserMessage

case object ShowSeries extends UserMessage

case object WrongMessage extends UserMessage
