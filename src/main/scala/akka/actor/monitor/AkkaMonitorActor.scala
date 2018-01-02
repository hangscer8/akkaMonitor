package akka.actor.monitor

import akka.actor.ActorPath
import akka.actor.baseActor._
import nathan.tree.{Branch, EmptyTree, Leaf, Tree}

import scala.concurrent.duration._

/**
  * Created by jianghang on 2017/12/28.
  */
class AkkaMonitorActor extends WrapperActor {
  var tree: Tree[ActorPath] = Leaf(self.path.root)
  var actorState = Map.empty[ActorPath, FetchAkkaMonitorStateResult]
  val GETAKKASTATE = "GETAKKASTATE"
  implicit val ec = context.system.dispatcher

  override def wrapPreStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[FetchResult])
    context.system.scheduler.schedule(3 seconds, 2 seconds, self, GETAKKASTATE)
    context.system.actorSelection("/user/*") ! FetchActorRef //第一步获取root节点下的 所有直接子节点
  }

  def genJsonTree() = {
    def loop(tree: Tree[ActorPath]): String = {
      tree match {
        case EmptyTree => ""
        case Leaf(v) =>
          actorState.get(v) match {
            case Some(targetState) =>
              s"""
                 |{
                 |"name":"${targetState.path}",
                 |"className":"${targetState.className}",
                 |"path":"${targetState.path}",
                 |"dealMsgNumber":${targetState.dealMsgNumber.toLongExact},
                 |"lastRunTime":${targetState.lastRunTime},
                 |"lastFinishTime":${targetState.lastFinishTime},
                 |"avgDealMsgDuration":${targetState.avgDealMsgDuration.doubleValue()},
                 |"constructTime":${targetState.constructTime},
                 |"children":[]
                 |}
           """.stripMargin
            case None =>
              s"""
                 |{
                 |"name":"${v}",
                 |"children":[]
                 |}
           """.stripMargin
          }

        case Branch(v, bs) =>
          actorState.get(v) match {
            case Some(targetState) =>
              s"""
                 |{
                 |"name":"${targetState.path}",
                 |"className":"${targetState.className}",
                 |"path":"${targetState.path}",
                 |"dealMsgNumber":${targetState.dealMsgNumber.toLongExact},
                 |"lastRunTime":${targetState.lastRunTime},
                 |"lastFinishTime":${targetState.lastFinishTime},
                 |"avgDealMsgDuration":${targetState.avgDealMsgDuration.doubleValue()},
                 |"constructTime":${targetState.constructTime},
                 |"children":[${bs.map(t => loop(t)).mkString(",")}]
                 |}
           """.stripMargin
            case None =>
              s"""
                 |{
                 |"name":"${v}",
                 |"children":[${bs.map(t => loop(t)).mkString(",")}]
                 |}
           """.stripMargin
          }
      }
    }

    loop(tree)
  }

  override def wrapReceive: Receive = {
    case FetchChildrenListResult(actor, childrenList) => //获取该actor的直接子节点，形成树
      tree = childrenList.foldRight(tree) { (ch, _tree) =>
        actor.path.parent.name match {
          case "user" | "singleton" | "remote" =>
            var tempTree = _tree
            tempTree = tempTree.insertChild(_ == actor.path.parent)(actor.path) //已经存在的节点不会重复insert
            tempTree = tempTree.insertChild(_ == actor.path)(ch.path)
            tempTree
          case _ =>
            var tempTree = _tree
            tempTree = tempTree.insertChild(_ == actor.path)(ch.path)
            tempTree
        }
      }
      childrenList.foreach(ch => ch ! FetchChildren)
      tree.print()
    case FetchActorRefResult(actor) => //第一步获取root节点下的 所有直接子节点
      tree = tree match { //需要把"/user"虚拟节点也加入tree里面来
        case Leaf(root) =>
          Branch(root, List(Branch(actor.path.parent, List(Leaf(actor.path))))) //root -> user(someNode.parent 虚拟节点) -> someNode
        case Branch(root, bs) =>
          Branch(root, bs.map(t => t.insertChild(_ == actor.path.parent)(actor.path)))
      }
      actor ! FetchChildren
    case FetchActorStarted(actor) => //监控每一个actor的创建
      tree = tree.insertChild(_ == actor.path.parent)(actor.path)
    case FetchActorTerminated(actor) => //监控每一个actor的销毁
      tree = tree.deleteSubTree(_ == actor.path)
    case stateRes: FetchAkkaMonitorStateResult =>
      actorState += (stateRes.path -> stateRes)
    case `GETAKKASTATE` =>
      tree.foreach { path =>
        context.actorSelection(path) ! FetchAkkaMonitorState
      }
    case "JSONTREE" =>
      sender() ! genJsonTree
  }
}

//{"name":'',"children":[
//
// ]}