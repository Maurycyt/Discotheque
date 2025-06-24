package cnf

// Sub-relational elements of a CNF formula.

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

// Higher-level elements of a CNF formula.
// Parametrised with what they allow as arguments to relations.
// CNFFormula[Term] allows functors, but CNFFormula[Variable] does not.

case class CNFFormula[T <: Term](name: String, clause: CNFClause[T]) {
	override def toString: String = s"$name: $clause"
}

case class CNFClause[T <: Term](literals: Set[CNFLiteral[T]]) {
	override def toString: String = literals.mkString(" | ")
}

case class CNFLiteral[T <: Term](negated: Boolean, relation: Relation[T]) {
	override def toString: String = {
		s"${if negated then "~" else ""}$relation"
	}
	
	def signedPredicate: (Boolean, String) = (negated, relation.name)
}

case class Relation[T <: Term](name: String, args: Vector[T]) {
	override def toString: String = s"$name${args.mkString("(", ",", ")")}"
}

def collectVarNames[T <: Term](clause: CNFClause[T]): Set[String] = {
	clause.literals.flatMap(_.relation.args.flatMap(collectVarNames))
}

def collectVarNames[T <: Term](term: T): Set[String] = term match {
	case v: Variable => Set(v.name)
	case c: Constant => Set.empty
	case f: Functor =>
		f.args.toSet.flatMap(collectVarNames)
}
