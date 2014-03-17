package com.glowingavenger.agent.util

import org.sat4j.scala.Logic._

class LogicExp(val symbols: Map[Symbol, Int], val cnf: List[List[Int]])

object LogicExp {
  implicit def boolExp2LogicExp(exp: BoolExp): LogicExp = {
    val (cnf, map) = encode[Symbol](exp)
    new LogicExp(map, cnf)
  }

  implicit def logicExp2BoolExp(exp: LogicExp): BoolExp = decode(exp.cnf, exp.symbols)
}