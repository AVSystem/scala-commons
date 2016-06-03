package com.avsystem.commons
package rpc.akka.client

import akka.actor.{Actor, ActorPath, Props}
import com.avsystem.commons.rpc.akka.ProcedureInvocationMessage

/**
  * @author Wojciech Milewski
  */
private[akka] class ProcedureClientActor(serverActorPath: ActorPath) extends Actor {
  override def receive: Receive = {
    case msg: ProcedureInvocationMessage =>
      context.actorSelection(serverActorPath) ! msg
  }
}

private[akka] object ProcedureClientActor {
  def props(serverActorPath: ActorPath): Props = Props(new ProcedureClientActor(serverActorPath))
}