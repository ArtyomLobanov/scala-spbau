package ru.spbau.jvm.scala.database

import akka.persistence.PersistentActor

import scala.collection.mutable
import scala.collection.immutable

class SeriesHolderActor extends PersistentActor {

  import SeriesHolderActor._

  val memory: mutable.HashMap[Long, mutable.HashMap[String, (Int, Int)]] =
    mutable.HashMap.empty
  val sessions: mutable.HashMap[Long, Option[String]] =
    mutable.HashMap.empty

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case GetSession(id) =>
      sender ! sessions.getOrElseUpdate(id, None)
    case GetEpisode(id, series) =>
      val userSeries = memory.getOrElseUpdate(id, mutable.HashMap.empty)
      sender ! (if (userSeries.contains(series)) Some(userSeries(series)) else None)
    case GetSeriesList(id) =>
      val userSeries = memory.getOrElseUpdate(id, mutable.HashMap.empty)
      sender ! immutable.HashMap(userSeries.toSeq: _*)
  }

  def receiveEvent(event: Event): Unit = {
    event match {
      case NewSeries(id, series) =>
        memory.getOrElseUpdate(id, mutable.HashMap.empty)(series) = (1, 1)
      case SetEpisode(id, series, season, episode) =>
        memory.getOrElseUpdate(id, mutable.HashMap.empty)(series) = (season, episode)
      case DeleteSeries(id, series) =>
        memory.getOrElseUpdate(id, mutable.HashMap.empty).remove(series)
      case SetSession(id, series) =>
        sessions(id) = series
      case _ =>
    }
  }

  override def persistenceId = "series-bot-holder-database"
}

object SeriesHolderActor {

  //events
  trait Event

  case class NewSeries(id: Long, series: String) extends Event

  case class DeleteSeries(id: Long, series: String) extends Event

  case class SetSession(id: Long, series: Option[String]) extends Event

  case class SetEpisode(id: Long, series: String, season: Int, episode: Int) extends Event

  case class GetSession(id: Long)

  case class GetEpisode(id: Long, series: String)

  case class GetSeriesList(id: Long)
}
