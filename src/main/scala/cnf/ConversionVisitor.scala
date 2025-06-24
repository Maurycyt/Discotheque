package cnf

import grammar.CNF.CNFBaseVisitor
import grammar.CNF.CNFParser.*
import scala.jdk.CollectionConverters.*

class ConversionVisitor extends CNFBaseVisitor[AnyRef] {
	override def visitCnfFormulaList(ctx: CnfFormulaListContext): Vector[CNFFormula[Term]] =
		ctx.cnfFormula.asScala.toVector.map(_.accept(this).asInstanceOf[CNFFormula[Term]])

	override def visitCnfFormula(ctx: CnfFormulaContext): CNFFormula[Term] =
		CNFFormula(
			ctx.name.getText,
			ctx.wrappedCnfClause.cnfClause.accept(this).asInstanceOf[CNFClause[Term]]
		)

	override def visitCnfClause(ctx: CnfClauseContext): CNFClause[Term] =
		CNFClause(ctx.cnfLiteral.asScala.toSet.map(_.accept(this).asInstanceOf[CNFLiteral[Term]]))

	override def visitLNamed(ctx: LNamedContext): AnyRef =
		CNFLiteral(ctx.Tilde != null, ctx.relation.accept(this).asInstanceOf[Relation[Term]])

	override def visitLComp(ctx: LCompContext): AnyRef =
		CNFLiteral(
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
