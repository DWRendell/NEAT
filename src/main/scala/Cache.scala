import scala.collection.mutable

/**
 * Wraps a function, and stores calculated outputs to avoid repeat calculations
 */
class Cache[I, O](function: I => O) {
  val cacheValues: mutable.Map[I, O] = mutable.Map.empty

  def apply(input: I): O = cacheValues.get(input) match {
    case Some(output) =>
      output
    case None =>
      val output = function(input)
      cacheValues += input -> output
      output
  }
}
