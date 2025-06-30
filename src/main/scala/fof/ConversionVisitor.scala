package fof

import common.*
import grammar.FOF.FOFBaseVisitor
import grammar.FOF.FOFParser.*

import scala.jdk.CollectionConverters.*

class ConversionVisitor extends FOFBaseVisitor[Vector[Formula[Term]]] {
	override def visitFofFormulaList(ctx: FofFormulaListContext): Vector[Formula[Term]] =
		ctx.fofFormula.asScala.toVector.map { ctx =>
			Formula(
				ctx.name.getText,
				ctx.formula.accept(this).asInstanceOf[Clause[Term]]
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
) extends FOFBaseVisitor[(Clause[Term], Map[String, String])] {

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

	override def aggregateResult(
		aggregate: (Clause[Term], Map[String, String]),
		nextResult: (Clause[Term], Map[String, String]),
	): (Clause[Term], Map[String, String]) = {
		val newQuantifiers = aggregate._1.quantifiers ++ nextResult._1.quantifiers
		val newLiterals = aggregate._1.literals ++ nextResult._1.literals
		(Clause(newQuantifiers, newLiterals), nextResult._2)
	}

	override def visitFQuantified(ctx: FQuantifiedContext): (Clause[Term], Map[String, String]) =
		ctx.formula.accept(withNames(ctx.variables.variable.asScala.toSet.map(_.name.getText)))
		// TODO: add quantifier colors to IR and provide them here

	override def visitFBinary(ctx: FBinaryContext): (Clause[Term], Map[String, String]) =
		ctx.BinaryOp.getSymbol.getText match {
			case "&" | "|" =>
				val r0 = ctx.formula(0).accept(this)
				val r1 = ctx.formula(1).accept(withMapping(r0._2))
				aggregateResult(r0, r1)
			case "=>" =>
				val r0 = ctx.formula(0).accept(notThis)
				val r1 = ctx.formula(1).accept(withMapping(r0._2))
				aggregateResult(r0, r1)
			case "<=" =>
				val r0 = ctx.formula(0).accept(this)
				val r1 = ctx.formula(1).accept(notThis.withMapping(r0._2))
				aggregateResult(r0, r1)
			case "<=>" =>
				val r0 = ctx.formula(0).accept(bothThis)
				val r1 = ctx.formula(1).accept(bothThis.withMapping(r0._2))
				aggregateResult(r0, r1)
		}

	override def visitFNegated(ctx: FNegatedContext): (Clause[Term], Map[String, String]) =
		ctx.formula.accept(notThis)

	override def visitFLiteral(ctx: FLiteralContext): (Clause[Term], Map[String, String]) =
		val newLiteral = ctx.literal.accept(LiteralVisitor(nameMapping)).asInstanceOf[Literal[Term]]
		mode match {
			case Mode.Positive =>
				(Clause(Map.empty, Set(newLiteral)), nameMapping)
			case Mode.Negative =>
				(Clause(Map.empty, Set(newLiteral.copy(negated = !newLiteral.negated))), nameMapping)
			case Mode.Both =>
				(
					Clause(Map.empty, Set(newLiteral, newLiteral.copy(negated = !newLiteral.negated))),
					nameMapping
				)
		}
}

class LiteralVisitor(nameMapping: Map[String, String]) extends FOFBaseVisitor[AnyRef] {
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
