package common

// Sub-relational elements of a theorem-like formula.

sealed trait Term

case class Constant(name: String) extends Term {
	override def toString: String = name
}

case class Variable(name: String) extends Term {
	override def toString: String = name
}

case class Functor(name: String, args: Vector[Term]) extends Term {
	override def toString: String = name ++ args.mkString("(", ",", ")")
}

// Higher-level elements of a formula.
// Parametrised with what they allow as arguments to relations.
// Formula[Term] allows functors, but Formula[Variable] does not.

case class Formula[T <: Term](name: String, clause: Clause[T]) {
	override def toString: String = s"$name: $clause"
}

case class Clause[T <: Term](quantifiers: Map[String, Quantifier], literals: Set[Literal[T]]) {
	override def toString: String = {
		import Quantifier.*
		(for (quantifier <- Seq(None, Universal, Existential, Clashing)) yield {
			val symbol = quantifier.toString
			symbol ++ quantifiers.toSeq.filter(_._2 == quantifier).map(_._1).mkString("(", ",", ")")
		}).mkString("") ++ " " ++ literals.mkString(" | ")
	}
}

case class Literal[T <: Term](signedPredicate: SignedPredicate, args: Vector[T]) {
	override def toString: String = {
		s"$signedPredicate${args.mkString("(", ",", ")")}"
	}

	def negated: Literal[T] = Literal(signedPredicate.negatedCopy, args)
}

case class SignedPredicate(negated: Boolean, name: String) {
	override def toString: String = s"${if negated then "~" else ""}$name"

	val isFunction: Boolean = name.last == '\''

	def negatedCopy: SignedPredicate = copy(negated = !negated)
}

object SignedPredicate {
	given Ordering[SignedPredicate] = Ordering.by(sp => (sp.negated, sp.name))
}

def collectVarNames[T <: Term](clause: Clause[T]): Set[String] = {
	clause.literals.flatMap(_.args.flatMap(collectVarNames))
}

def collectVarNames[T <: Term](term: T): Set[String] = term match {
	case v: Variable => Set(v.name)
	case c: Constant => Set.empty
	case f: Functor =>
		f.args.toSet.flatMap(collectVarNames)
}

// Fills in the missing quantifiers to be None.
def correctQuantifiers(clause: Clause[Variable]): Clause[Variable] = {
	val vars = common.collectVarNames(clause)
	val unquantifiedVars = vars -- clause.quantifiers.keySet
	val correctedClause = clause.copy(
		quantifiers = clause.quantifiers ++ unquantifiedVars.map { n => (n, common.Quantifier.None) }
	)
	correctedClause
}
