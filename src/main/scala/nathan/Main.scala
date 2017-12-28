package nathan

import akka.actor.baseActor._
import akka.actor.monitor.AkkaMonitorActor
import akka.actor.{ActorRef, ActorSystem, Props}

import scala.concurrent.Await
import akka.pattern._
import akka.util.Timeout
import nathan.tree.{Branch, Leaf, Tree}

import scala.concurrent.duration._

object Main extends App {
  implicit val timeout = Timeout(13 seconds)
  val system = ActorSystem("akkaMonitor")
  val genActor = system.actorOf(Props(classOf[GenActor], 4))
  system.actorOf(Props[AkkaMonitorActor], "monitorCenter")
  Thread.sleep(2000)
  system.stop(genActor)

  def getChildrenList(actorRef: ActorRef): FetchChildrenListResult = {
    Await.result((actorRef ? FetchChildren).mapTo[FetchChildrenListResult], 10 seconds)
  }

  def genTree(actorRef: ActorRef): Tree[ActorRef] = getChildrenList(actorRef).childrenList match {
    case Nil =>
      Leaf(actorRef)
    case children =>
      Branch(actorRef, children.map(ch => genTree(ch)))
  }
}

class GenActor(n: Int) extends WrapperActor {
  override def wrapPreStart(): Unit = n match {
    case 0 =>
    case _ => (1 to n).foreach(_ => context.actorOf(Props(classOf[GenActor], n - 1)))
  }

  override def wrapReceive: Receive = {
    case _ =>
  }
}