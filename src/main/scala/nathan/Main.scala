package nathan

import akka.actor.baseActor._
import akka.actor.monitor.AkkaMonitorActor
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import akka.pattern._

import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Main extends App {
  implicit val timeout = Timeout(13 seconds)
  implicit val ec = scala.concurrent.ExecutionContext.global
  val system = ActorSystem("akkaMonitor")
  val genActor = system.actorOf(Props(classOf[GenActor], 2))
  val monitorActor = system.actorOf(Props[AkkaMonitorActor], "monitorCenter")
  (1 to 10).foreach { index =>
    Thread.sleep(1000)
    println(index)
  }
  (monitorActor ? "JSONLIST").mapTo[String] onComplete {
    case Success(r) => println(r)
    case Failure(ex) =>
      ex.printStackTrace()
  }
}

class GenActor(n: Int) extends WrapperActor {
  override def wrapPreStart(): Unit = n match {
    case 0 =>
    case _ => (1 to n).foreach { _ =>
      context.actorOf(Props(classOf[GenActor], n - 1))
    }
  }

  override def wrapReceive: Receive = {
    case _ =>
  }
}