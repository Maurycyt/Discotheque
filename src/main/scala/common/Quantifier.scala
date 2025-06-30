package common

trait Quantifier {
	def clashesWith(other: Quantifier): Boolean

	def combine(other: Quantifier): Quantifier
}

object Quantifier {
	case object None extends Quantifier {
		override def clashesWith(other: Quantifier): Boolean = other == Clashing

		override def combine(other: Quantifier): Quantifier = other

		override def toString: String = "@"
	}

	case object Universal extends Quantifier {
		override def clashesWith(other: Quantifier): Boolean = other match {
			case Existential | Clashing => true
			case _ => false
		}

		override def combine(other: Quantifier): Quantifier =
			if clashesWith(other) then Clashing else this

		override def toString: String = "!"
	}

	case object Existential extends Quantifier {
		override def clashesWith(other: Quantifier): Boolean = other match {
			case Universal | Clashing => true
			case _ => false
		}

		override def combine(other: Quantifier): Quantifier =
			if clashesWith(other) then Clashing else this

		override def toString: String = "?"
	}

	case object Clashing extends Quantifier {
		override def clashesWith(other: Quantifier): Boolean = true

		override def combine(other: Quantifier): Quantifier = this

		override def toString: String = "↯"
	}
}
