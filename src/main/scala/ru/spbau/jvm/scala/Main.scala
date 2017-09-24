package ru.spbau.jvm.scala

import akka.actor.{ActorSystem, Props}
import ru.spbau.jvm.scala.bot.SeriesHolderBot
import ru.spbau.jvm.scala.database.SeriesHolderActor


object Main extends App {
  val token = "405765344:AAHU4uyiiQr58MG6m832LUCyzIdHKWbzKzI"

  val system = ActorSystem()
  val database = system.actorOf(Props(classOf[SeriesHolderActor]))
  private val bot = new SeriesHolderBot(token, database)

  bot.run()
}
