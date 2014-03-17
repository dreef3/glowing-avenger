package com.glowingavenger.agent

import org.sat4j.scala.Logic._
import scalax.collection.Graph
import scalax.collection.GraphPredef._,
scalax.collection.GraphEdge._
import language.{higherKinds, postfixOps}
import scalax.collection.{Graph, GraphLike}
import scalax.collection.GraphPredef._,
scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCompanion
import PlaneEdgeImplicits._
import scala.collection.SortedMap
import com.glowingavenger.agent.util.ASearch

object Plan {
  val DefaultCost = 1
  val DefaultAsk = (s: Symbol) => Some(true)

  class Search(problem: Problem) extends ASearch[ProducedState] {
    override def isGoal(node: ProducedState, goal: ProducedState): Boolean = goal includes node

    override def heruisticCost(init: ProducedState, goal: ProducedState): Int = init.countEqual(goal)

    override def cost(node: ProducedState): Int = DefaultCost

    override def successors(node: ProducedState): List[ProducedState] = {
      Plan.this.successors(node, problem.actions) map (edge => new ProducedState(edge._2.attrs, edge.action))
    }
  }

  private def successors(state: BeliefState, actions: List[Action]): List[PlanEdge[BeliefState]] = {
    val pairs = for {
      action <- actions
      result = action.result(state)
      if result != state
    } yield (result, action)
    // TODO Implicit conversion returns type List[Any] instead of List[PlanEdge]
    pairs.map(p => p._1 match {
      case Left(b) => PlanEdge(state, b, p._2) :: Nil
      case Right((b1, b2)) => PlanEdge(state, b1, p._2) :: PlanEdge(state, b2, p._2) :: Nil
    }).flatten
  }

  def build(problem: Problem): Option[Plan] = {
    val searcher = new Search(problem)

    def makeEdges(seq: List[ProducedState]): List[PlanEdge[BeliefState]] = {
      if (seq.tail.isEmpty)
        Nil
      else
        List(PlanEdge(seq.head, seq.tail.head, seq.tail.head.producer)) ++ makeEdges(seq.tail)
    }

    def produce(state: BeliefState):Option[IntermediateResult] = {
      searcher.search(state, problem.goal) match {
        case None => None
        case Some(seq) => {
          val guaranteed = makeEdges(seq)
          val ask = successors(state, state.unknown map (AskAction(_, DefaultAsk)))
          val frontier = ask map (_._2)
          val edges = guaranteed ++ ask
          Some(IntermediateResult(Graph(edges: _*), frontier))
        }
      }
    }

    None
  }

  case class IntermediateResult(graph: Graph[BeliefState, PlanEdge], frontier: List[BeliefState])
}

class Plan(val problem: Problem, val steps: List[Action])

case class AskAction(attr: Symbol, doAsk: Symbol => Option[Boolean]) extends Action {
  override def result(state: BeliefState): (BeliefState, BeliefState) = {
    (new BeliefState(state.attrs ++ Map(attr -> Some(true))), new BeliefState(state.attrs ++ Map(attr -> Some(false))))
  }

  override def applicableIn(state: BeliefState): Boolean = {
    state.attrs.contains(attr) && state.attrs(attr) == None
  }

  override def attrs: List[Symbol] = attr :: Nil

  override val name: String = AskAction.getClass.getSimpleName + attr
}

case class NoAction() extends Action {
  override def result(state: BeliefState): BeliefState = state

  override def applicableIn(state: BeliefState): Boolean = false

  override def attrs: List[Symbol] = Nil

  override val name: String = NoAction.getClass.getSimpleName
}

case class TryAction(override val name: String, options: Map[Action, Int]) extends Action {
  private val sortedOptions = SortedMap(options.toSeq: _*)
  override def result(state: BeliefState): Either[BeliefState, (BeliefState, BeliefState)] = doTry(state, sortedOptions)

  private def doTry(state: BeliefState, options: SortedMap[Action, Int]): Either[BeliefState, (BeliefState, BeliefState)] = {
    if (options.isEmpty)
      Left(state)
    else options.head._1.result(state) match {
      case s if s == state => doTry(state, options.tail)
      case _ => _
    }
  }

  override def applicableIn(state: BeliefState): Boolean = options.keys.exists(_.applicableIn(state))

  override def attrs: List[Symbol] = (options.keys.head.attrs /: options.keys.tail)(_ ++ _.attrs).toSet.toList
}
