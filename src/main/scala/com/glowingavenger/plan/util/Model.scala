package com.glowingavenger.plan.util

import org.sat4j.scala.Logic._
import scala.Some

object Model {
  private val ExtraLiteral = 'ExtraLiteral

  /**
   * Checks which symbols in the model are actually inferred from the clause
   */
  def retrieveSymbols(model: Map[Symbol, Option[Boolean]], clause: BoolExp): Option[Map[Symbol, Option[Boolean]]] = {
    retrieveModels(clause) match {
      case None => None
      case Some(clauseModel) => Some(model map {
        p => (p._1, if (clauseModel contains p._1) clauseModel(p._1) else None)
      })
    }
  }

  def retrieveModelsOrThrow(clause: BoolExp): Map[Symbol, Option[Boolean]] = {
    Model.retrieveModels(clause) match {
      case Some(models) => models
      case None => throw new IllegalArgumentException("Clause is not satisfiable: " + clause)
    }
  }

  def retrieveModels(clause: BoolExp, extraLiteral:Option[Symbol] = None): Option[Map[Symbol, Option[Boolean]]] = {
    def withoutExtra(model: Map[Symbol, Boolean]) = {
      extraLiteral match {
        case Some(l) => model - l
        case _ => model
      }
    }
    allSat[Symbol](clause) match {
      case (true, models) if models.isDefined => Some(parseModels(models.get.map(withoutExtra)))
      // For the case of trivial clause such as 'L & 'L allSat doesn't produce a single model
      case (false, Some(_)) =>
        retrieveModels(clause & ExtraLiteral, Some(ExtraLiteral))
      case _ => None
    }
  }

  private def parseModels(models: List[Map[Symbol, Boolean]]): Map[Symbol, Option[Boolean]] = {
    val list = for (model <- models; pair <- model) yield pair
    list groupBy (_._1) map (p => {
      val set = p._2.unzip._2.toSet
      (p._1, if (set.count(b => true) > 1) None else Some(set.head))
    })
  }

  def model2BoolExp(model: Map[Symbol, Option[Boolean]]): BoolExp = (True.asInstanceOf[BoolExp] /: model)((e, p) => e & (p match {
    case (s, None) => s | ~s
    case (s, Some(true)) => toProp(s)
    case (s, Some(false)) => ~s
  }))
}
