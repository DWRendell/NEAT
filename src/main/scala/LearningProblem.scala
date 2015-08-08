/**
 * Represents a problem we aim to solve using NEAT.
 */
trait LearningProblem {
  val inputs: Int
  val outputs: Int

  def measureFitness(network: NeuralNetwork): Double
}
