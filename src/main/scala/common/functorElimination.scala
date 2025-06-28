package common

import scala.annotation.tailrec

// Algorithm to convert a CNFFormula[Term] to a CNFFormula[Variable].
// For every functor `f(t1,t2,...,tn)`, replace its occurrence with variable `S`
// and add a relation element `~ f'(t1,t2,...,tn,S)`.
// For example, the formula `~r(A,B,c) | s(f(d,B),C)`
// will be converted to `~c'(N0) | ~d'(N1) | ~f'(N1,B,N2) | ~r(A,B,N0) | s(N2,C)` (or equivalent).

// Eliminates functors in a formula.
def eliminateFunctors(formula: Formula[Term]): Formula[Variable] = Formula(
	formula.name, common.eliminateFunctors(formula.clause)
)

// Eliminates functors in a clause.
def eliminateFunctors(clause: Clause[Term]): Clause[Variable] = {
	val varNames = collectVarNames(clause)
	val (resultLiterals, resultUsedNames) = clause.literals.foldLeft((Set.empty[Literal[Variable]], varNames)) {
		case ((accLiterals, usedNames), literal) =>
			val (newAccLiterals, newUsedNames, newArgs) = eliminateFunctors(
				accLiterals, usedNames, literal.relation.args
			)
			(
				newAccLiterals + Literal(
					negated = literal.negated,
					relation = Relation(name = literal.relation.name, args = newArgs)
				),
				newUsedNames
			)
	}
	Clause(resultLiterals)
}

// Eliminates functors in a list of terms (arguments to a functor or relation).
// Takes and returns an accumulator of literals and used variable names.
// Returns new list of variable names.
def eliminateFunctors(
	accLiterals: Set[Literal[Variable]], usedNames: Set[String], args: Vector[Term]
): (Set[Literal[Variable]], Set[String], Vector[Variable]) = {
	val (resultAccLiterals, resultUsedNames, resultArgs) = args
		.foldLeft((accLiterals, usedNames, List.empty[Variable])) {
			case ((accLiterals, usedNames, accArgs), arg) =>
				val (newAccLiterals, newUsedNames, newArg) = eliminateFunctor(
					accLiterals, usedNames, arg
				)
				(newAccLiterals, newUsedNames, newArg :: accArgs)
		}
	(resultAccLiterals, resultUsedNames, resultArgs.reverse.toVector)
}

// Eliminates functor from a single term, if applicable.
// Takes and returns an accumulator of literals and used variable names.
// Returns new variable name to use in place of functor.
@tailrec
def eliminateFunctor(
	accLiterals: Set[Literal[Variable]], usedNames: Set[String], arg: Term
): (Set[Literal[Variable]], Set[String], Variable) = arg match {
	case v: Variable => (accLiterals, usedNames, v)
	case c: Constant => eliminateFunctor(accLiterals, usedNames, Functor(c.name, Vector.empty))
	case f: Functor =>
		val (newAccLiterals, newUsedNames, newArgs) = eliminateFunctors(
			accLiterals, usedNames, f.args
		)
		val newVarName = getNewName(newUsedNames)
		(
			newAccLiterals + Literal(
				negated = true,
				relation = Relation(name = f.name ++ "'", args = newArgs :+ Variable(newVarName))
			),
			newUsedNames + newVarName,
			Variable(newVarName)
		)
}

def getNewName(names: Set[String]): String = {
	var i = 0
	while names.contains(s"N$i") do i += 1
	s"N$i"
}