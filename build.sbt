name := "akkamonitorSystem"

version := "0.1"

scalaVersion := "2.12.4"
cancelable in Global := true
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.4",
  "com.typesafe.akka" % "akka-actor_2.12" % "2.4.19",
  "com.typesafe.akka" %% "akka-cluster" % "2.4.19",
  "com.typesafe.akka" %% "akka-cluster-tools" % "2.4.19"
)
        