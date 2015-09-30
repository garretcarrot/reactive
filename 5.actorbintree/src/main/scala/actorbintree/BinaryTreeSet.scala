/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import actorbintree.BinaryTreeNode._
import actorbintree.BinaryTreeSet._
import akka.actor._

import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  var pendingQueue = Queue.empty[Operation]

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case contains @ Contains(requester, id, requestedElem) => root ! contains
    case insert @ Insert(requester, id, insertedElem) => root ! insert
    case remove @ Remove(requester, id, removedElem) => root ! remove
    case GC =>
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
  }

  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation => pendingQueue :+= op
    case CopyFinished =>
      pendingQueue foreach (op => newRoot ! op)
      pendingQueue = Queue.empty
      root ! PoisonPill
      root = newRoot
      context.become(normal)
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def receive = normal

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case contains @ Contains(requester, id, requestedElem) =>
      if (requestedElem < elem) {
        subtrees.get(Left).fold(requester ! ContainsResult(id, result = false)) { child =>
          child ! contains
        }
      } else if (requestedElem > elem) {
        subtrees.get(Right).fold(requester ! ContainsResult(id, result = false)) { child =>
          child ! contains
        }
      } else {
        requester ! ContainsResult(id, result = !removed)
      }

    case insert @ Insert(requester, id, insertedElem) =>
      if (insertedElem < elem) {
        subtrees.get(Left) match {
          case Some(child) => child ! insert
          case None =>
            subtrees += Left -> context.actorOf(BinaryTreeNode.props(insertedElem, initiallyRemoved = false))
            requester ! OperationFinished(id)
        }
      } else if (insertedElem > elem) {
        subtrees.get(Right) match {
          case Some(child) => child ! insert
          case None =>
            subtrees += Right -> context.actorOf(BinaryTreeNode.props(insertedElem, initiallyRemoved = false))
            requester ! OperationFinished(id)
        }
      } else {
        removed = false
        requester ! OperationFinished(id)
      }

    case remove @ Remove(requester, id, removedElem) =>
      if (removedElem < elem) {
        subtrees.get(Left) match {
          case Some(child) => child ! remove
          case None => requester ! OperationFinished(id)
        }
      } else if (removedElem > elem) {
        subtrees.get(Right) match {
          case Some(child) => child ! remove
          case None => requester ! OperationFinished(id)
        }
      } else {
        removed = true
        requester ! OperationFinished(id)
      }

    case copy @ CopyTo(treeNode) =>
      context.become(copying(subtrees.values.toSet, insertConfirmed = removed))
      subtrees.values foreach (node => node ! copy)
      if (removed && subtrees.isEmpty) {
        context.parent ! CopyFinished
      } else if (!removed) {
        treeNode ! Insert(self, self.hashCode(), elem)
      }

  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) =>
      context.become(copying(expected, insertConfirmed = true))
      if (expected.isEmpty) {
        context.parent ! CopyFinished
      }

    case CopyFinished =>
      val stillExpected = expected - context.sender()
      context.become(copying(stillExpected, insertConfirmed))
      if (stillExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
      }
  }


}
