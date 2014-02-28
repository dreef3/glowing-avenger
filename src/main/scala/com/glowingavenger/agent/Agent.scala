package com.glowingavenger.agent

import org.sat4j.scala.Logic._

class LogicExp(val symbols: Map[Symbol, Int], val cnf: List[List[Int]])

object LogicExp {
  implicit def boolExp2LogicExp(exp: BoolExp): LogicExp = {
    val (cnf, map) = encode[Symbol](exp)
    new LogicExp(map, cnf)
  }

  implicit def logicExp2BoolExp(exp: LogicExp): BoolExp = decode(exp.cnf, exp.symbols)
}

class Action(val precond: BoolExp, val effect: BoolExp, val exec: () => Unit, val name: String)
object Action {
  def apply(precond: BoolExp, effect: BoolExp, exec: () => Unit = () => (), name: String = "") = new Action(precond, effect, exec, name)
}

class Agent(val symbols: List[Symbol], val actions: List[Action], initial: Option[BoolExp], val name: String = "") {

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
      retrieveSymbols(allSatModels.head._1, init)
    } else if (satGoalModels.isEmpty) {
      None
    } else {
      backwardAction(init, satGoalModels.head._3.precond) match {
        case None => None
        case something => {
          satGoalModels.head._3.exec()
          something
        }
      }
    }
  }

  /**
   * Checks which symbols in the model are actually inferred from init
   */
  def retrieveSymbols(model: Map[Symbol, Option[Boolean]], clause: BoolExp): Option[Map[Symbol, Option[Boolean]]] = {
    retrieveModels(clause & axiom) match {
      case None => None
      case Some(initModel) => Some(model map {
        p => (p._1, if (initModel contains p._1) initModel(p._1) else None)
      })
    }
  }

  def retrieveModels(clause: BoolExp): Option[Map[Symbol, Option[Boolean]]] = {
    allSat[Symbol](clause) match {
      case (true, models) if models.isDefined => Some(parseModels(models.get))
      // For the case of trivial clause such as 'L & 'L allSat doesn't produce a single model
      case (false, Some(_)) => isSat[Symbol](clause) match {
        case (true, model) if model.isDefined => Some(parseModels(model.get :: Nil))
        case _ => None
      }
      case _ => None
    }
  }

  def parseModels(models: List[Map[Symbol, Boolean]]): Map[Symbol, Option[Boolean]] = {
    val list = for (model <- models; pair <- model) yield pair
    list groupBy (_._1) map (p => {
      val set = p._2.unzip._2.toSet
      // TODO Eliminate call to count
      (p._1, if (set.count(b => true) > 1) None else Some(set.head))
    })
  }

  def model2BoolExp(model: Map[Symbol, Option[Boolean]]): BoolExp = (True.asInstanceOf[BoolExp] /: model)((e, p) => e & (p match {
    case (s, None) => s | ~s
    case (s, Some(true)) => toProp(s)
    case (s, Some(false)) => ~s
  }))
}