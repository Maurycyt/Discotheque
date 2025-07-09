package fof

import common.*
import grammar.CNFnFOF.CNFnFOFBaseVisitor
import grammar.CNFnFOF.CNFnFOFParser.*

import scala.jdk.CollectionConverters.*

class ConversionVisitor extends CNFnFOFBaseVisitor[Vector[Formula[Variable]]] {
	override def visitFormulaList(ctx: FormulaListContext): Vector[Formula[Variable]] =
		ctx.formulaEntry.asScala.toVector.map { ctx =>
			Formula(
				ctx.name.getText,
				ctx.formula.accept(FormulaToClauseVisitor())._1
			)
		}
}

object FormulaToClauseVisitor {
	enum Mode {
		case Positive
		case Negative
		case Both
	}
}

/**
 * Converts a formula in the FOF grammar to a clause. Also returns a mapping of variable names used
 * to ensure that each logical variable has a unique name in the resulting clause.
 *
 * @param mode        Whether to add literals positively, negatively, or both.
 * @param nameMapping A mapping of variable names to ensure uniqueness in the resulting clause.
 *                    It is required that all reserved names appear as keys.
 */
class FormulaToClauseVisitor(
	mode: FormulaToClauseVisitor.Mode = FormulaToClauseVisitor.Mode.Positive,
	nameMapping: Map[String, String] = Map.empty,
) extends CNFnFOFBaseVisitor[(Clause[Variable], Map[String, String])] {

	import FormulaToClauseVisitor.*

	private def notThis: FormulaToClauseVisitor = {
		mode match {
			case Mode.Positive => FormulaToClauseVisitor(Mode.Negative, nameMapping)
			case Mode.Negative => FormulaToClauseVisitor(Mode.Positive, nameMapping)
			case Mode.Both => this
		}
	}

	private def bothThis: FormulaToClauseVisitor = FormulaToClauseVisitor(Mode.Both, nameMapping)

	private def withMapping(newNameMapping: Map[String, String]): FormulaToClauseVisitor = {
		FormulaToClauseVisitor(mode, newNameMapping)
	}

	private def withNames(names: Set[String]): FormulaToClauseVisitor = {
		def generateNewName(nameMapping: Map[String, String], name: String): String = {
			var newName = name
			while nameMapping.contains(newName) do {
				newName = newName + "'"
			}
			newName
		}

		var newNameMapping = nameMapping
		for name <- names do {
			newNameMapping += (name -> generateNewName(nameMapping, name))
		}

		withMapping(newNameMapping)
	}

	private def customCombineResult(
		aggregate: (Clause[Variable], Map[String, String]),
		nextResult: (Clause[Variable], Map[String, String]),
	): (Clause[Variable], Map[String, String]) = {
		val newQuantifiers = aggregate._1.quantifiers ++ nextResult._1.quantifiers
		val newLiterals = aggregate._1.literals ++ nextResult._1.literals
		(Clause(newQuantifiers, newLiterals), nextResult._2)
	}

	override def visitFormulaEntry(ctx: FormulaEntryContext): (Clause[Variable], Map[String, String]) = {
		if ctx.language.getText == "fof" then
			ctx.formula.accept(this)
		else {
			val variables = ctx.formula.accept(new VariableCollector())
			ctx.formula.accept(withMapping(variables.map { n => (n, n) }.toMap))
		}
	}

	override def visitFWrapped(ctx: FWrappedContext): (Clause[Variable], Map[String, String]) =
		ctx.formula.accept(this)

	override def visitFQuantified(ctx: FQuantifiedContext): (Clause[Variable], Map[String, String]) = {
		val newNames = ctx.variables.variable.asScala.toSet.map(_.name.getText)
		val (clause, nameMapping) = ctx.formula.accept(withNames(newNames))
		val quantifier =
			if ctx.quantifier.getText == "!"
			then common.Quantifier.Universal else common.Quantifier.Existential
		val newQuantifiers = clause.quantifiers
			++ newNames.map { name => (nameMapping(name), quantifier) }.toMap
		(clause.copy(quantifiers = newQuantifiers), nameMapping)
	}

	override def visitFBinary(ctx: FBinaryContext): (Clause[Variable], Map[String, String]) =
		ctx.BinaryOp.getSymbol.getText match {
			case "&" | "|" =>
				val r0 = ctx.formula(0).accept(this)
				val r1 = ctx.formula(1).accept(withMapping(r0._2))
				customCombineResult(r0, r1)
			case "=>" =>
				val r0 = ctx.formula(0).accept(notThis)
				val r1 = ctx.formula(1).accept(withMapping(r0._2))
				customCombineResult(r0, r1)
			case "<=" =>
				val r0 = ctx.formula(0).accept(this)
				val r1 = ctx.formula(1).accept(notThis.withMapping(r0._2))
				customCombineResult(r0, r1)
			case "<=>" =>
				val r0 = ctx.formula(0).accept(bothThis)
				val r1 = ctx.formula(1).accept(bothThis.withMapping(r0._2))
				customCombineResult(r0, r1)
		}

	override def visitFNegated(ctx: FNegatedContext): (Clause[Variable], Map[String, String]) =
		ctx.formula.accept(notThis)

	override def visitFLiteral(ctx: FLiteralContext): (Clause[Variable], Map[String, String]) =
		// Parse the literal
		val literal = ctx.literal.accept(LiteralVisitor(nameMapping)).asInstanceOf[Literal[Term]]

		// Eliminate the functors
		val usedNames = nameMapping.keySet
		val (functorLiterals, newUsedNames, defunctoredArguments) =
			common.eliminateFunctors(Set.empty, usedNames, literal.relation.args)
		val newLiteral = Literal(
			literal.negated,
			Relation(literal.relation.name, defunctoredArguments)
		)
		val extraUsedNames = newUsedNames -- usedNames
		val newNameMapping = nameMapping ++ extraUsedNames.map { name => (name, name) }

		// Return a clause with the new functor literals and the new name mapping.
		// Add the defunctored literal in the correct mode.
		(
			Clause(
				Map.empty,
				functorLiterals ++
					(mode match {
						case Mode.Positive => Set(newLiteral)
						case Mode.Negative => Set(newLiteral.copy(negated = !newLiteral.negated))
						case Mode.Both => Set(newLiteral, newLiteral.copy(negated = !newLiteral.negated))
					})
			),
			newNameMapping
		)
}

class LiteralVisitor(nameMapping: Map[String, String]) extends CNFnFOFBaseVisitor[AnyRef] {
	override def visitLNamed(ctx: LNamedContext): Literal[Term] = {
		Literal(false, ctx.relation.accept(this).asInstanceOf[Relation[Term]])
	}

	override def visitLComp(ctx: LCompContext): Literal[Term] =
		Literal(
			ctx.comp.getText == "!=",
			Relation(
				"=",
				ctx.term.asScala.toVector.map(_.accept(this).asInstanceOf[Term])
			)
		)

	override def visitRelation(ctx: RelationContext): AnyRef =
		Relation(
			ctx.name.getText,
			ctx.termList.term.asScala.toVector.map(_.accept(this).asInstanceOf[Term])
		)

	override def visitTFunctor(ctx: TFunctorContext): Term =
		Functor(
			ctx.functor.name.getText,
			ctx.functor.termList.term.asScala.toVector.map(_.accept(this).asInstanceOf[Term])
		)

	override def visitTConstant(ctx: TConstantContext): Term =
		Constant(ctx.constant.name.getText)

	override def visitTVariable(ctx: TVariableContext): Term =
		Variable(nameMapping(ctx.variable.name.getText))
}

class VariableCollector extends CNFnFOFBaseVisitor[Set[String]] {
	override def defaultResult = Set.empty[String]

	override def aggregateResult(aggregate: Set[String], nextResult: Set[String]): Set[String] = {
		aggregate ++ nextResult
	}

	override def visitTVariable(ctx: TVariableContext): Set[String] = Set(ctx.variable.name.getText)
}
