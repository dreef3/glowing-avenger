package com.glowingavenger.plan.importexport

import org.scalatest.{Matchers, FlatSpec}
import PDDLParser._
import org.sat4j.scala.Logic._
import scala.io.Source
import com.glowingavenger.plan.model.{Problem, UnboundProblem, Domain}
import com.glowingavenger.plan.model.action.LogicAction

class PDDLParserSpec extends FlatSpec with Matchers {
  behavior of "Problem import from PDDL"

  private def parse[T](input: String)(implicit p: Parser[T]): T = {
    val parser = phrase(p)
    parser(new PackratReader[PDDLParser.Elem](new lexical.Scanner(input))) match {
      case Success(t: T, _) => t
      case NoSuccess(msg, i) =>
        val a = i
        throw new IllegalArgumentException(msg)
    }
  }

  it should "parse a predicate" in {
    val r = parse("(L) ;some comment")(predicate)
    r.name shouldBe "L"
  }

  it should "parse the predicates list" in {
    val r = parse("(:predicates\n(L) ;Лампа горит\n(B) ;Лампа исправна\n(S) ;Выключатель включен\n(C) ;Кондиционер включен\n(W) ;Окно открыто\n\t)")(predicatesDef)
    r shouldBe List('L, 'B, 'S, 'C, 'W)
  }

  it should "parse a logic expression" in {
    val r = parse("(B & S)")(logicExpr)
    r shouldBe ('B & 'S)
  }

  it should "parse the unknown term" in {
    parse("L?")(formula) shouldBe 'L | ~'L
  }

  it should "parse an axiom" in {
    val r = parse("(:axiom (B & S)) ;some comment")(axiomDef)
    r shouldBe 'B & 'S
  }

  it should "parse an action" in {
    val r = parse("(:action off_light ;Выключить выключатель\n:precondition (S?)\n:effect (~S)\n\t)")(actionDef)
    val a = LogicAction('S?, ~'S, "off_light")
    r shouldBe a
  }

  it should "parse the domain PDDL definition" in {
    val parsed = parse(Source.fromFile("src/test/resources/Bulb_domain.pddl").mkString)(domain)
    parsed shouldBe Domain(List('L, 'B, 'S, 'C, 'W),
      List(LogicAction('S?, ~'S, "off_light"), LogicAction('S?, 'S & 'L.?, "on_light")), List('L iff ('B & 'S)), "bulb")
  }

  it should "parse the problem PDDL definition" in {
    val parsed = parse(Source.fromFile("src/test/resources/Bulb_problem.pddl").mkString)(problem)
    parsed shouldBe UnboundProblem("bulb", ~'L, 'L, "switch_on")
  }

  it should "parse the full PDDL definition" in {
    val parsed = parse(Source.fromFile("src/main/resources/Bulb.pddl").mkString)(pddl)
    parsed shouldBe Problem(Domain(List('L, 'B, 'S, 'C, 'W), List(LogicAction('S?, ~'S, "off_light"),
        LogicAction('S?, 'S & ('L?), "on_light"), LogicAction(('B?) & ~'S, 'B, "ch_bulb"),
        LogicAction('C?, ~'C, "off_cond"), LogicAction('C?, 'C, "on_cond"),
        LogicAction('W?, ~'W, "off_wind"), LogicAction('W?, 'W, "on_wind")),
        List('L iff ('B & 'S)), "bulb"),
      ~'L, 'L, "switch_on")
  }

  it should "parse Alarm domain definition" in {
    val parsed = parse(Source.fromFile("src/test/resources/Alarm_domain.pddl").mkString)(domain)
    parsed should not be null
  }

  it should "parse full Alarm definition" in {
    val parsed = parse(Source.fromFile("src/main/resources/Alarm.pddl").mkString)(pddl)
    parsed should not be null
  }
}
