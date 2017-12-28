package akka.actor.monitor

import akka.actor.ActorRef
import akka.actor.baseActor._

/**
  * Created by jianghang on 2017/12/28.
  */
class AkkaMonitorActor extends WrapperActor {
  var systemSubNode = List.empty[ActorRef]

  override def wrapPreStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[FetchResult])
    context.system.actorSelection("/user/*") ! FetchActorRef //第一步获取root节点下的 所有直接子节点
  }

  override def wrapReceive: Receive = {
    case FetchChildrenListResult(actor, childrenList) =>

    case FetchActorRefResult(otherActor) =>
      systemSubNode ::= otherActor

    case FetchActorStarted(actor) =>

    case FetchActorTerminated(actor) =>

  }
}
