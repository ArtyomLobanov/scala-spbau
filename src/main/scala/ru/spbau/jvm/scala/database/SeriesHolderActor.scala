package ru.spbau.jvm.scala.database

import akka.persistence.PersistentActor

import scala.collection.mutable

class SeriesHolderActor extends PersistentActor {

  import SeriesHolderActor._

  val memory: mutable.HashMap[Long, mutable.HashMap[String, SeriesInfo]] =
    mutable.HashMap.empty
  val sessions: mutable.HashMap[Long, Option[Session]] =
    mutable.HashMap.empty

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case GetSession(id) =>
      sender ! sessions.getOrElseUpdate(id, None)
    case GetSeriesInfo(id, series) =>
      val userSeries = memory.getOrElseUpdate(id, mutable.HashMap.empty)
      sender ! (if (userSeries.contains(series)) Some(userSeries(series)) else None)
    case GetSeriesList(id) =>
      val userSeries = memory.getOrElseUpdate(id, mutable.HashMap.empty)
      sender ! userSeries.toSeq.map(pair => pair._2).toList
  }

  def receiveEvent(event: Event): Unit = {
    event match {
      case PutSeriesInfo(id, info) =>
        memory.getOrElseUpdate(id, mutable.HashMap.empty)(info.name) = info
      case DeleteSeries(id, series) =>
        memory.getOrElseUpdate(id, mutable.HashMap.empty).remove(series)
      case SetSession(id, maybeSession) =>
        sessions(id) = maybeSession
    }
  }

  override def persistenceId = "series-bot-database"
}

case class SeriesInfo(name: String, season: Int, episode: Int) {
  override def toString: String = {
    s"$name - сезон $season, эпизод $episode"
  }
}

case class Session(series: String)

object SeriesHolderActor {

  //events
  sealed trait Event

  case class DeleteSeries(id: Long, series: String) extends Event

  case class SetSession(id: Long, series: Option[Session]) extends Event

  case class PutSeriesInfo(id: Long, info: SeriesInfo) extends Event

  case class GetSession(id: Long)

  case class GetSeriesInfo(id: Long, series: String)

  case class GetSeriesList(id: Long)
}
