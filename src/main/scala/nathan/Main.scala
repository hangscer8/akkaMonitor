package nathan

import akka.actor.baseActor._
import akka.actor.monitor.AkkaMonitorActor
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration._

object Main extends App {
  implicit val timeout = Timeout(13 seconds)
  val system = ActorSystem("akkaMonitor")
  val genActor = system.actorOf(Props(classOf[GenActor], 4))
  println(genActor.path.parent.name) // user
  println(genActor.path.root)
  system.actorOf(Props[AkkaMonitorActor], "monitorCenter")
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

//ActorRef Actor[akka://akkaMonitor/user/$a#-2066925243]
//path akka://akkaMonitor/user/$a
//path.name $a
//path.address akka://akkaMonitor
//path.root akka://akkaMonitor/