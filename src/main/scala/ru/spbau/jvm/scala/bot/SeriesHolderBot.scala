package ru.spbau.jvm.scala.bot

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.models.Message
import ru.spbau.jvm.scala.database.SeriesHolderActor._
import ru.spbau.jvm.scala.database.{SeriesInfo, Session}
import ru.spbau.jvm.scala.parser._

import scala.concurrent.duration.DurationInt
import scala.util.Success


class SeriesHolderBot(val token: String,
                      val database: ActorRef) extends TelegramBot with Polling with Commands {

  def showAllSeries()(implicit message: Message): Unit = {
    implicit val timeout: Timeout = Timeout(1.second)
    (database ? GetSeriesList(message.chat.id)).onComplete {
      case Success(list: List[SeriesInfo]) =>
        val result = new StringBuilder()
        result.append("Ваши сериалы:\n")
        list.foreach(info => result.append(s"   $info\n"))
        if (list.isEmpty) {
          reply("У вас нет сериалов!")
        } else {
          reply(result.toString)
        }
      case _ =>
        reply("У вас нет сериалов!")
    }
  }

  def putSeriesInfo(series: String, season: Int, episode: Int)(implicit message: Message): Unit = {
    val updatedInfo = SeriesInfo(series, season, episode)
    database ! PutSeriesInfo(message.chat.id, updatedInfo)
    reply(s"Информация обновлена ($updatedInfo)")
  }

  def forgetSeries(series: String)(implicit message: Message): Unit = {
    val chatId = message.chat.id
    ifSeriesDefined(chatId, series,
      _ => {
        ifCurrentSeriesDefined(chatId,
          currentSeries => {
            if (currentSeries.name == series) {
              reply("Сначала завершите сессию")(message)
            } else {
              deleteSeriesInfo(chatId, series)
            }
          },
          () => deleteSeriesInfo(chatId, series))
      },
      () => reply("Этот сериал не в ходит в список ваших сериалов")(message))
  }

  def ifCurrentSeriesDefined(chatId: Long, successBody: (SeriesInfo => Unit),
                             failureBode: (() => Unit))(implicit message: Message): Unit = {
    implicit val timeout: Timeout = Timeout(1.second)
    (database ? GetSession(chatId)).onComplete {
      case Success(Some(session: Session)) =>
        ifSeriesDefined(chatId, session.series, successBody, failureBode)
      case _ =>
        failureBode()
    }
  }

  def ifSeriesDefined(chatId: Long, series: String, successBody: (SeriesInfo => Unit),
                      failureBode: (() => Unit))(implicit message: Message): Unit = {
    implicit val timeout: Timeout = Timeout(1.second)
    (database ? GetSeriesInfo(chatId, series)).onComplete {
      case Success(Some(info: SeriesInfo)) =>
        successBody(info)
      case _ =>
        failureBode()
    }
  }

  def deleteSeriesInfo(chatId: Long, series: String)(implicit message: Message): Unit = {
    database ! DeleteSeries(chatId, series)
    reply(s"Информация о сериале ($series) удалена")
  }

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        MessageParser.parse(text) match {
          case ForgetSeries(series) =>
            forgetSeries(series)
          case StartSession(series) =>
            ifCurrentSeriesDefined(message.chat.id,
              _ => reply("Сначала завершите предыдущую сессию!"),
              () => {
                database ! SetSession(message.chat.id, Some(Session(series)))
                reply(s"Сессия начата ($series)")
              })
          case StopSession =>
            ifCurrentSeriesDefined(message.chat.id,
              seriesInfo => {
                database ! SetSession(message.chat.id, None)
                reply(s"Сессия завершена (${seriesInfo.name})")
              },
              () => reply("Нет активной сессии!"))
          case NextEpisode =>
            ifCurrentSeriesDefined(message.chat.id,
              seriesInfo => putSeriesInfo(seriesInfo.name, seriesInfo.season, seriesInfo.episode + 1),
              () => reply("Нет активной сессии!"))
          case NextSeason =>
            ifCurrentSeriesDefined(message.chat.id,
              seriesInfo => putSeriesInfo(seriesInfo.name, seriesInfo.season + 1, 1),
              () => reply("Нет активной сессии!"))
          case DefineEpisode(series: String, season: Int, episode: Int) =>
            ifSeriesDefined(message.chat.id, series,
              _ => putSeriesInfo(series, season, episode),
              () => reply("Неизвестный сериал!"))
          case AddSeries(series) =>
            ifSeriesDefined(message.chat.id, series,
              _ => reply("Вы уже смотрите этот сериал!"),
              () => putSeriesInfo(series, 1, 1))
          case RemindEpisode(series: String) =>
            ifSeriesDefined(message.chat.id, series,
              seriesInfo => reply(s"$series - ${seriesInfo.season} сезон ${seriesInfo.episode} серия"),
              () => reply("Неизвестный сериал!"))
          case ShowSeries =>
            showAllSeries()
          case WrongMessage =>
            reply("Неверная команда")
        }
      }
  }
}
