package matching

class ExhaustibleSet[T](elements: Set[T] = Set.empty) extends IterableOnce[T] {
	private var exhausted: Set[T] = Set.empty

	private def this(elements: IterableOnce[T], exhausted: Set[T]) = {
		this(elements.iterator.toSet)
		this.exhausted = exhausted
	}

	private class ExhaustibleIterator(eSet: ExhaustibleSet[T]) extends Iterator[T] {
		private val elementIterator = elements.iterator.filterNot(exhausted.contains)

		override def hasNext: Boolean = elementIterator.hasNext && exhausted.size < elements.size

		override def next(): T = {
			if !hasNext then throw new NoSuchElementException("No more elements in the ExhaustibleSet")
			val nextElement = elementIterator.next()
			exhausted += nextElement
			nextElement
		}
	}

	override def iterator: Iterator[T] = ExhaustibleIterator(this)

	def addAll(newElements: IterableOnce[T]): ExhaustibleSet[T] = {
		val elementsToAdd = newElements.iterator.filterNot(exhausted.contains).toSet
		ExhaustibleSet(elements ++ elementsToAdd, exhausted)
	}

	def copy: ExhaustibleSet[T] = ExhaustibleSet(elements, exhausted)
}

//class ClauseMatcherBacktrackingController extends IterableOnce{
//
//}
