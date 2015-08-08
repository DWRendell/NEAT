object NetworkSpec extends App {
  val input1 = Node(1)
  val input2 = Node(2)
  val biasNode = Node(0)
  val connection1 = input1 connectTo biasNode
  val connection2 = input2 connectTo biasNode
  val network = NeuralNetwork(List(input1, input2, biasNode), List(connection1, connection2))

  val problem = new XorLearningProblem
  println(problem.measureFitness(network))
}
