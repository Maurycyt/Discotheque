package common

trait Semigroup[T] {
	infix def combine(other: T): T
}
