object NetworkSpec extends App {
  val input1 = Node(1)
  val input2 = Node(2)
  val output = Node(3)
  val connection1 = input1 connectTo output
  val connection2 = input2 connectTo output
  val network = NeuralNetwork(2, 1)

  val problem = new XorLearningProblem
  println(problem.measureFitness(network))
}
