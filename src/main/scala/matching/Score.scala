package matching

/**
 * Contains all the scoring information for a matching of two clauses.
 *
 * @param totalRelations    The total number of relations in the matching. This is the sum of the relations in both clauses.
 * @param validRelations    The number of valid relations in the matching. These are relations that have a corresponding match in the other clause.
 * @param invalidRelations  The number of invalid relations in the matching. These are relations that are saturated (all their arguments are matched), but do not have a corresponding match in the other clause.
 * @param variableUnions    The number of unions between variables made for the matching.
 * @param quantifierClashes The number of variable equivalence classes having mismatched quantifiers, whether within themselves or with their match.
 *                          // @param quantifierDependencyViolations The number of invalid quantifier dependencies...?
 */
case class Score(
	totalRelations: Int,
	validRelations: Int,
	invalidRelations: Int,
	variableUnions: Int,
	quantifierClashes: Int,
	//	quantifierDependencyViolations: Int,
) {
	// The total weighted score.
	def score(cfg: ScoringConfig): Double = {
		import cfg.*
		validRelationReward * validRelations - invalidRelationPenalty * invalidRelations -
			variableUnionPenalty * variableUnions - quantifierClashPenalty * quantifierClashes
	}

	// The relative score, normalized by the hypothetical total number of points available
	// from all the relations, multiplied by the reward for a valid relation.
	def relativeScore(cfg: ScoringConfig): Double =
		score(cfg) / totalRelations * cfg.validRelationReward
}

case class ScoringConfig(
	validRelationReward: Double,
	invalidRelationPenalty: Double,
	variableUnionPenalty: Double,
	quantifierClashPenalty: Double,
)
