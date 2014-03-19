package com.glowingavenger.agent.plan

import com.glowingavenger.agent.problem._
import scalax.collection.Graph
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LBase._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.WLDiEdge
import scala.collection.immutable.Queue
import com.glowingavenger.agent.util.ASearch

object SimplePlan {
  def apply(problem: Problem) = new SimplePlan(problem)

  def build(problem: Problem) = SimplePlan(problem).build()
}

class SimplePlan(val problem: Problem) {
  val guaranteed = new ASearch[(BeliefState, Action)]{
    override def isGoal(node: (BeliefState, Action), goal: (BeliefState, Action)): Boolean = goal._1 includes node._1

    override def hCost(init: (BeliefState, Action), goal: (BeliefState, Action)): Int = 0

    override def cost(node: (BeliefState, Action)): Int = 1

    override def successors(node: (BeliefState, Action)): List[(BeliefState, Action)] = for {
      action <- problem.actions
      result = action.result(node._1)
      if result != node._1
    } yield (result, action)
  }

  def build(): Graph[BeliefState, WLDiEdge] = {
    val front = Queue(problem.init)
    val empty = Graph[BeliefState, WLDiEdge]()
    buildRec(front, empty)
  }

  private def buildRec(front: Queue[BeliefState], plan: Graph[BeliefState, WLDiEdge]): Graph[BeliefState, WLDiEdge] = {
    if (front.isEmpty) plan
    else {
      val (next, nextQueue) = front.dequeue
      val askers = askSuccessors(next)
      val additions = askers.unzip._1.toIterable
      buildRec(nextQueue ++ additions, plan ++ (pathToGoal(next) ::: list2Edges(next, askers)))
    }
  }

  private def pathToGoal(state: BeliefState, producer: Action = NoAction()) = {
    guaranteed.search((state, producer), (problem.goal, NoAction())) match {
      case Some(path) => path2Edges(path)
      case None => Nil
    }
  }

  private def list2Edges(from: BeliefState, successors: List[(BeliefState, Action)]): List[WLDiEdge[BeliefState]] = {
    for {
      (to, action) <- successors
    } yield (from ~%+> to)(action.cost, action)
  }
  
  private def path2Edges(path: List[(BeliefState, Action)]): List[WLDiEdge[BeliefState]] = {
    if (path.isEmpty || path.tail.isEmpty)
      Nil
    else
      (path.head._1 ~%+> path.tail.head._1)(path.tail.head._2.cost, path.tail.head._2) :: Nil ::: path2Edges(path.tail)
  }

  private def askSuccessors(state: BeliefState): List[(BeliefState, Action)] = {
    state.unknown.map(AskAction).map {
      ask =>
        ask.result(state) match {
          case a: Answer => (a.yes, ask) :: (a.no, ask) :: Nil
          case _ => Nil
        }
    }.flatten
  }
}