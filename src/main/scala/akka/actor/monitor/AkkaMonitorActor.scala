package akka.actor.monitor

import akka.actor.ActorPath
import akka.actor.baseActor._
import nathan.tree.{Branch, Leaf, Tree}

/**
  * Created by jianghang on 2017/12/28.
  */
class AkkaMonitorActor extends WrapperActor {
  var tree: Tree[ActorPath] = Leaf(self.path.root)

  override def wrapPreStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[FetchResult])
    context.system.actorSelection("/user/*") ! FetchActorRef //第一步获取root节点下的 所有直接子节点
  }

  override def wrapReceive: Receive = {
    case FetchChildrenListResult(actor, childrenList) => //获取该actor的直接子节点，形成树
      tree = childrenList.foldRight(tree) { (ch, _tree) =>
        _tree.insertChild(_ == actor.path)(ch.path)
      }
      childrenList.foreach(ch => ch ! FetchChildren)
    case FetchActorRefResult(actor) => //第一步获取root节点下的 所有直接子节点
      tree = tree match {
        case Leaf(root) =>
          Branch(root, List(Leaf(actor.path)))
        case Branch(root, bs) =>
          Branch(root, Leaf(actor.path) :: bs)
      }
      actor ! FetchChildren
    case FetchActorStarted(actor) => //监控每一个actor的创建
      tree = tree.insertChild(_ == actor.path.parent)(actor.path)
    case FetchActorTerminated(actor) => //监控每一个actor的销毁
      tree = tree.deleteSubTree(_ == actor.path)
  }
}