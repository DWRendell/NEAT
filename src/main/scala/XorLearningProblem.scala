import breeze.numerics.abs

class XorLearningProblem {
  val inputs = 2
  val outputs = 1

  def measureFitness(network: NeuralNetwork) = {
    assert(network.inputNodes.length == inputs &&
           network.outputNodes.length == outputs)

    val testInputs = Seq((0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0))
    val targetOutputs = Seq(0.0, 1.0, 1.0, 0.0)

    val actualOutputs = testInputs.map(x => network(x._1, x._2).head)

    val diffs = actualOutputs.zip(targetOutputs).map(x => abs(x._1 - x._2))

    4.0 - diffs.foldLeft(0.0)(_+_)
  }
}
