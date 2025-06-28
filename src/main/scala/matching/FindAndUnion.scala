package matching

// Find-and-union structure for equivalence classes.
class FindAndUnion(val n: Int) {
	private val parent = Array.tabulate(n)(identity)
	private var quotientSize = n

	// Finds the representative of the equivalence class containing x.
	// Uses path compression.
	def find(x: Int): Int = {
		if parent(x) != x then parent(x) = find(parent(x))
		parent(x)
	}

	// Joins the equivalence classes of x and y.
	private def union(x: Int, y: Int): Unit = {
		val rx = find(x)
		val ry = find(y)
		if rx == ry then return
		parent(rx) = ry
		quotientSize -= 1
	}

	// Copy the find-and-union structure, with additional unions.
	def withUnions(unions: (Int, Int)*): FindAndUnion = {
		val result = FindAndUnion(n)
		parent.copyToArray(result.parent)
		result.quotientSize = quotientSize
		for ((x, y) <- unions) {
			result.union(x, y)
		}
		result
	}

	// Returns the current size of the quotient set.
	def getQuotientSize: Int = quotientSize

	// Copies the parent array to the provided array.
	def copyParentsTo(array: Array[Int]): Unit = {
		parent.copyToArray(array)
	}
}
