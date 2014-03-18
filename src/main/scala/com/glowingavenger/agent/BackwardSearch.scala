package com.glowingavenger.agent

import org.sat4j.scala.Logic._
import com.glowingavenger.agent.util.Model
import Model._
import com.glowingavenger.agent.problem.LogicAction

class BackwardSearch(val symbols: List[Symbol], val actions: List[LogicAction], initial: Option[BoolExp], val name: String = "") {

  val axiom = if (initial.isDefined) initial.get else True

  /**
   * Depth-first search for actions with any models which satisfy a provided goal.
   * Then constructs a sequence of actions where each next is required to satisfy the condition of the previous one.
   * The process continues until the space of symbols used in actions' effects and preconditions is closed, that is,
   * no more new symbols were added for the last required action in the sequence.
   *
   * Such space of symbols is then checked individually to be satisfiable against current (initial) state and provided axioms.
   *
   * @param goal logic expression which describes a goal in terms of this Agent's symbols.
   * @return a sub-sequence of this Agent's symbols which are required to be inferred in order to achieve the goal
   *         along with their inferred values (if it's possible)
   */
  def backwardSearch(init: BoolExp, goal: BoolExp): Option[Map[Symbol, Option[Boolean]]] = {
    val clause = axiom & init & goal
    retrieveModels(clause) match {
      case None => backwardAction(init, goal)
      case something => something
    }
  }

  private def backwardAction(init: BoolExp, goal: BoolExp): Option[Map[Symbol, Option[Boolean]]] = {
    val actionModels = for {
      action <- actions
      goalClause = axiom & goal & action.effect
      initClause = axiom & init & action.precond
      goalModels = retrieveModels(goalClause)
      initModels = retrieveModels(initClause)
    } yield (initModels, goalModels, action)
    val satGoalModels = actionModels filter (ig => ig._2.isDefined) map (ig => (ig._1, ig._2.get, ig._3))
    val allSatModels = satGoalModels filter (ig => ig._1.isDefined) map (ig => (ig._1.get, ig._2, ig._3))
    if (!allSatModels.isEmpty) {
      retrieveSymbols(allSatModels.head._1, init & axiom)
    } else if (satGoalModels.isEmpty) {
      None
    } else {
      backwardAction(init, satGoalModels.head._3.precond) match {
        case None => None
        case something => {
          something
        }
      }
    }
  }
}