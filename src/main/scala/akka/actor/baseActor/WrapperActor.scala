package akka.actor.baseActor

import akka.actor.Actor

/**
  * Created by jianghang on 17/12/27.
  */
trait WrapperActor extends Actor {

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

  def wrapReceive: Receive

  def fetchResult: ReceiveFetchState = {
    case FetchChildren =>
      sender() ! context.children.toList
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
  override def preStart(): Unit = wrapPreStart()

  /**
    * 请使用wrapPostStop
    */
  @Deprecated
  override def postStop(): Unit = wrapPostStop()

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