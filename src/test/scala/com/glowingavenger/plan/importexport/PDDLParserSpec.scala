package com.glowingavenger.plan.importexport

import org.scalatest.{Matchers, FlatSpec}
import PDDLParser._
import org.sat4j.scala.Logic._
import scala.io.Source

class PDDLParserSpec extends FlatSpec with Matchers {
  behavior of "Problem import from PDDL"

  private def parse[T](input: String)(implicit p: Parser[T]): T = {
    val parser = phrase(p)
    parser(new PackratReader[PDDLParser.Elem](new lexical.Scanner(input))) match {
      case Success(t: T, _) => t
      case _ => throw new IllegalArgumentException()
    }
  }

  it should "parse a simple attribute" in {
    parse("(L=\"Some value\")")(attr) shouldBe ("L", "Some value")
  }

  it should "parse a definition identifier" in {
    parse("action ident")(defIdent) shouldBe ("action", "ident")
  }

  it should "parse a boolean formula" in {
    parse("A & B")(formula) shouldBe ('A & 'B)
  }

  it should "parse a knowledge base attribute" in {
    parse("(: init A & B)")(problemKBaseAttr) shouldBe Some(("init", 'A & 'B))
  }

  it should "parse a domain attribute" in {
    parse("(: domain domain1)")(domainAttr) shouldBe Some("domain1")
  }

  it should "parse domain predicates" in {
    val r = parse("(:predicates\n\t\t(L=\"Лампа горит\")\n\t\t(B=\"Лампа исправна\")\n\t)")(predicates)
    r  shouldBe Some(List(("L", "Лампа горит"), ("B", "Лампа исправна")))
  }

  it should "parse a domain definition" in {
    val r = parse(Source.fromFile("src/test/resources/Bulb_domain.pddl").mkString)(domain)
    r shouldBe Some(DomainStub("bulb", Map('L -> "Лампа горит", 'B -> "Лампа исправна"),
      Map("off_light" -> "Выключить выключатель", "on_light" -> "Включить выключатель"), List('L iff ('B & 'S))))
  }

  it should "parse a problem definition" in {
    val r = parse(Source.fromFile("src/test/resources/Bulb_problem.pddl").mkString)(problem)
    r shouldBe Some(ProblemStub("switch_on", "bulb", ~'L, 'L))
  }

  

/*  it should "parse a PDDL dsl" in {
    val r = parse(Source.fromFile("src/test/resources/Bulb.pddl").mkString)(dsl)
  }*/
}
