package cnf

import common.*
import grammar.CNF.CNFBaseVisitor
import grammar.CNF.CNFParser.*

import scala.jdk.CollectionConverters.*

class ConversionVisitor extends CNFBaseVisitor[AnyRef] {
	override def visitCnfFormulaList(ctx: CnfFormulaListContext): Vector[Formula[Term]] =
		ctx.cnfFormula.asScala.toVector.map(_.accept(this).asInstanceOf[Formula[Term]])

	override def visitCnfFormula(ctx: CnfFormulaContext): Formula[Term] =
		Formula(
			ctx.name.getText,
			ctx.wrappedCnfClause.cnfClause.accept(this).asInstanceOf[Clause[Term]]
		)

	override def visitCnfClause(ctx: CnfClauseContext): Clause[Term] =
		val unquantified = Clause(
			Map.empty, ctx.cnfLiteral.asScala.toSet.map(_.accept(this).asInstanceOf[Literal[Term]])
		)
		val variableNames = collectVarNames(unquantified)
		Clause(
			Map.from(variableNames.map((_, Quantifier.None))),
			ctx.cnfLiteral.asScala.toSet.map(_.accept(this).asInstanceOf[Literal[Term]])
		)

	override def visitLNamed(ctx: LNamedContext): AnyRef =
		Literal(ctx.Tilde != null, ctx.relation.accept(this).asInstanceOf[Relation[Term]])

	override def visitLComp(ctx: LCompContext): AnyRef =
		Literal(
			ctx.comp.getText == "!=",
			Relation(
				"=",
				ctx.term.asScala.toVector.map(
					_.accept(this).asInstanceOf[Term]
				)
			)
		)

	override def visitRelation(ctx: RelationContext): Relation[Term] =
		Relation(
			ctx.name.getText,
			ctx.termList.term.asScala.toVector.map(_.accept(this).asInstanceOf[Term])
		)

	override def visitTConstant(ctx: TConstantContext): Term =
		Constant(ctx.constant.name.getText)

	override def visitTVariable(ctx: TVariableContext): Term =
		Variable(ctx.variable.name.getText)

	override def visitTFunctor(ctx: TFunctorContext): Term = {
		Functor(
			ctx.functor.name.getText,
			ctx.functor.termList.term.asScala.toVector.map(_.accept(this).asInstanceOf[Term])
		)
	}
}
