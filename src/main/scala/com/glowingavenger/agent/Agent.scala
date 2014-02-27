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

class Action(val precond: LogicExp, val effect: LogicExp, val name: String = "")
class Agent(val symbols: List[Symbol], val actions: List[Action], val name: String = "") {

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
  def backwardSearch(goal: LogicExp): List[(Symbol, Option[Boolean])] = {
    require(goal.symbols.keys.find(s => !(symbols contains s)) == None)

/*
    val satisfiable = actions map (a => (a, isSat(a.effect & goal))) groupBy (_._2) find (_._1._1) match {
      case Some(x) => x._2(0)._1
      case _ => throw new IllegalArgumentException
    }
*/
  }
}