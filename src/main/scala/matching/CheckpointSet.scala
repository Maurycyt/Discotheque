package matching

/**
 * A CheckpointSet is a set of elements that can be iterated over and expanded, but supports
 * the functionality of creating another CheckpointSet from its iterator, which remembers the
 * elements it already iterated over and will not add them or iterate over them again.
 * @param elements The initial set of elements.
 * @tparam T The type of elements in the set.
 */
class CheckpointSet[T](private val elements: Set[T] = Set.empty) extends Iterable[T] {
	private var exhausted: Set[T] = Set.empty

	def this(elements: Set[T], exhausted: Set[T]) = {
		this(elements)
		this.exhausted = exhausted
	}

	class ExhaustibleIterator(cSet: CheckpointSet[T]) extends Iterator[T] {
		private var exhausted: Set[T] = cSet.exhausted

		private val elementIterator = cSet.elements.iterator.filterNot(exhausted.contains)

		override def hasNext: Boolean = elements.size > exhausted.size

		override def next(): T = {
			val nextElement = elementIterator.next()
			exhausted += nextElement
			nextElement
		}

		def checkpoint: CheckpointSet[T] =
			if exhausted eq cSet.exhausted then cSet else CheckpointSet(cSet.elements, exhausted)
	}

	override def iterator: ExhaustibleIterator = ExhaustibleIterator(this)

	def addAll(newElements: IterableOnce[T]): CheckpointSet[T] = {
		val elementsToAdd = newElements.iterator.filterNot(exhausted.contains).toSet
		CheckpointSet(elements ++ elementsToAdd, exhausted)
	}

	override def size: Int = elements.size - exhausted.size
}
