package common

trait Quantifier {
	def clashesWith(other: Quantifier): Boolean
}

object Quantifier {
	case object None extends Quantifier {
		override def clashesWith(other: Quantifier): Boolean = false
	}

	case object Universal extends Quantifier {
		override def clashesWith(other: Quantifier): Boolean = other match {
			case Existential => true
			case _ => false
		}
	}

	case object Existential extends Quantifier {
		override def clashesWith(other: Quantifier): Boolean = other match {
			case Universal => true
			case _ => false
		}
	}
}
