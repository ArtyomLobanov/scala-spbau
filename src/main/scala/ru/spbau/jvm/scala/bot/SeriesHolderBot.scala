package ru.spbau.jvm.scala.bot

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.jvm.scala.database.SeriesHolderActor.{GetEpisode, _}
import ru.spbau.jvm.scala.parser._

import scala.collection.immutable
import scala.concurrent.duration.DurationInt
import scala.util.Success


class SeriesHolderBot(val token: String,
                      val database: ActorRef) extends TelegramBot with Polling with Commands {


  onMessage {
    implicit message =>
      message.text.foreach { text =>
        MessageParser.parse(text) match {
          case AddSeries(series) =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetEpisode(message.chat.id, series)).onComplete {
              case Success(Some(_)) =>
                reply("Вы уже смотрите этот сериал!")
              case _ =>
                database ! NewSeries(message.chat.id, series)
                reply("Приятного просмотра!")
            }
          case ForgetSeries(series) =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetEpisode(message.chat.id, series)).onComplete {
              case Success(Some(_)) =>
                implicit val timeout: Timeout = Timeout(1.second)
                (database ? GetSession(message.chat.id)).onComplete {
                  case Success(Some(session)) =>
                    if (session == series) {
                      reply("Сначала завершите сессию")
                    } else {
                      database ! DeleteSeries(message.chat.id, series)
                      reply("Информация о сериале удалена")
                    }
                  case _ =>
                    database ! DeleteSeries(message.chat.id, series)
                    reply("Информация о сериале удалена")
                }
              case _ =>
                reply("Этот сериал не в ходит в список ваших сериалов")
            }
          case DefineEpisode(series: String, season: Int, episode: Int) =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetEpisode(message.chat.id, series)).onComplete {
              case Success(Some(_)) =>
                database ! SetEpisode(message.chat.id, series, season, episode)
                reply("Информация обновлена")
              case _ =>
                reply("Неизвестный сериал!")
            }
          case StartSession(series) =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetSession(message.chat.id)).onComplete {
              case Success(Some(session)) =>
                if (session != series) {
                  database ! SetSession(message.chat.id, Some(series))
                  reply("Предыдущая сессия завершена (" + session + ")" +
                    "\nСессия начата (" + series + ")")
                } else {
                  reply("Эта сессия и так активна")
                }
              case _ =>
                database ! SetSession(message.chat.id, Some(series))
                reply("Сессия начата (" + series + ")")
            }
          case StopSession =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetSession(message.chat.id)).onComplete {
              case Success(Some(session)) =>
                database ! SetSession(message.chat.id, None)
                reply("Сессия завершена (" + session + ")")
              case _ =>
                reply("Нет активной сессии")
            }
          case NextEpisode =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetSession(message.chat.id)).onComplete {
              case Success(Some(session: String)) =>
                implicit val timeout: Timeout = Timeout(1.second)
                (database ? GetEpisode(message.chat.id, session)).onComplete {
                  case Success(Some((season: Int, episode: Int))) =>
                    database ! SetEpisode(message.chat.id, session, season, episode + 1)
                    reply("Информация обновлена")
                  case _ =>
                    reply("Неизветная ошибка")
                }
              case _ =>
                reply("Нет активной сессии")
            }
          case NextSeason =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetSession(message.chat.id)).onComplete {
              case Success(Some(session: String)) =>
                implicit val timeout: Timeout = Timeout(1.second)
                (database ? GetEpisode(message.chat.id, session)).onComplete {
                  case Success(Some((season: Int, episode: Int))) =>
                    database ! SetEpisode(message.chat.id, session, season + 1, 1)
                    reply("Информация обновлена")
                  case _ =>
                    reply("Неизветная ошибка")
                }
              case _ =>
                reply("Нет активной сессии")
            }
          case RemindEpisode(series: String) =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetEpisode(message.chat.id, series)).onComplete {
              case Success(Some((season: Int, episode: Int))) =>
                reply(series + " - " + season + " сезон " + episode + " серия")
              case _ =>
                reply("Неизвестный сериал!")
            }
          case ShowSeries =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetSeriesList(message.chat.id)).onComplete {
              case Success(list: immutable.HashMap[String, (Int, Int)]) =>
                val result = new StringBuilder()
                result.append("Ваши сериалы:\n")
                list.foreach(x =>
                  result.append("   " + x._1 + "  (s" + x._2._1 + " e" + x._2._2 + ")\n"))
                if (list.isEmpty) {
                  reply("У вас нет сериалов!")
                } else {
                  reply(result.toString)
                }
              case _ =>
                reply("У вас нет сериалов!")
            }
          case WrongMessage =>
            reply("Неверная команда")
        }
      }
  }
}
