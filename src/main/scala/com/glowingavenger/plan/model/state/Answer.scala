package com.glowingavenger.plan.model.state

case class Answer(override val predicates: Map[Symbol, Option[Boolean]], attr: Symbol) extends BeliefState(predicates) {
  require(predicates.keySet.contains(attr))
  require(predicates(attr) == None)

  val yes = BeliefState(predicates ++ Map(attr -> Some(true)))
  val no = BeliefState(predicates ++ Map(attr -> Some(false)))

  override def equals(other: Any): Boolean = other match {
    case that: Answer =>
      (that canEqual this) &&
        predicates == that.predicates &&
        attr == that.attr
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(predicates, attr)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
