name := "scala-2017"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "info.mukel" %% "telegrambot4s" % "3.0.9",
  "com.typesafe.akka" %% "akka-persistence" % "2.4.19",
  "com.enragedginger" %% "akka-quartz-scheduler" % "1.6.0-akka-2.4.x",
  "org.iq80.leveldb" % "leveldb" % "0.7",
  "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)