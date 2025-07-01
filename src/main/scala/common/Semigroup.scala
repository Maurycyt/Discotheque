package common

trait Semigroup[T] {
	def combine(other: T): T
}
