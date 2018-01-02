package akka.actor.baseActor

import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef}

/**
  * Created by jianghang on 17/12/27.
  */
trait WrapperActor extends Actor with ActorLogging {

  import WrapperActor.ReceiveFetchState

  /**
    * 已经处理的消息数量(处理失败的消息数量)
    */
  private[this] var dealMsgNumber: BigDecimal = 0.0
  /**
    * receive函数上次开始运行时刻与结束时刻
    */
  private[this] var lastRunTime: Long = -1L
  private[this] var lastFinishTime: Long = -1L
  /**
    * 每个消息的平均处理时间
    */
  private[this] var avgDealMsgDuration: BigDecimal = 0.0
  /**
    * actor新建构造的时刻
    */
  private[this] var constructTime: Long = -1L

  def wrapReceive: Receive

  def fetchResult: ReceiveFetchState = {
    case FetchAkkaMonitorState =>
      val stateRes = FetchAkkaMonitorStateResult(self, self.path, this.getClass.getName, dealMsgNumber, lastRunTime, lastFinishTime, avgDealMsgDuration, constructTime)
      context.system.eventStream.publish(stateRes)
    case FetchChildren =>
      context.system.eventStream.publish(FetchChildrenListResult(self, context.children.toList))
    case FetchActorRef =>
      context.system.eventStream.publish(FetchActorRefResult(self))
  }

  @throws(classOf[Exception])
  def wrapPreStart(): Unit = ()

  @throws(classOf[Exception])
  def wrapPostStop(): Unit = ()

  @throws(classOf[Exception])
  def wrapPreRestart(reason: Throwable, message: Option[Any]): Unit = {
    context.children foreach { child ⇒
      context.unwatch(child)
      context.stop(child)
    }
    wrapPostStop()
  }

  def wrapPostRestart(reason: Throwable): Unit = wrapPreStart()

  /**
    * 请使用wrapReceive
    */
  @Deprecated
  override def receive: Receive = {
    case msg: FetchSate =>
      fetchResult(msg)
    case msg =>
      dealMsgNumber += 1
      lastRunTime = System.currentTimeMillis()
      wrapReceive(msg)
      lastFinishTime = System.currentTimeMillis()
      avgDealMsgDuration = (avgDealMsgDuration * (dealMsgNumber - 1) + lastFinishTime - lastRunTime) / dealMsgNumber
  }

  /**
    * 请使用wrapPreStart
    */
  @Deprecated
  override def preStart(): Unit = {
    constructTime = System.currentTimeMillis()
    context.system.eventStream.publish(FetchActorStarted(self))
    wrapPreStart()
  }

  /**
    * 请使用wrapPostStop
    */
  @Deprecated
  override def postStop(): Unit = {
    context.system.eventStream.publish(FetchActorTerminated(self))
    wrapPostStop()
  }

  /**
    * 请使用wrapPreRestart
    */
  @Deprecated
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = wrapPreRestart(reason, message)

  /**
    * 请使用wrapPostRestart
    */
  @Deprecated
  override def postRestart(reason: Throwable): Unit = wrapPostRestart(reason)
}

object WrapperActor {
  type ReceiveFetchState = PartialFunction[FetchSate, Unit]
}

trait FetchSate

case object FetchAkkaMonitorState extends FetchSate

case object FetchChildren extends FetchSate

case object FetchActorRef extends FetchSate

trait FetchResult

case class FetchAkkaMonitorStateResult(actor: ActorRef, path: ActorPath, className: String, dealMsgNumber: BigDecimal, lastRunTime: Long, lastFinishTime: Long, avgDealMsgDuration: BigDecimal, constructTime: Long) extends FetchResult

case class FetchActorTerminated(actor: ActorRef) extends FetchResult

case class FetchActorStarted(actor: ActorRef) extends FetchResult

case class FetchChildrenListResult(actor: ActorRef, childrenList: List[ActorRef]) extends FetchResult

case class FetchActorRefResult(actor: ActorRef) extends FetchResult