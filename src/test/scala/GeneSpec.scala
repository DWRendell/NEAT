object GeneSpec extends App {
  val geneSec = new GeneSequence(2, 1)
    .withGene(NewConnectionGene(Node(1), Node(3)))
    .withGene(NewConnectionGene(Node(2), Node(3)))
    .withGene(SplitConnectionGene(Connection(Node(1), Node(3)), Node(4)))

  println(new XorLearningProblem().measureFitness(geneSec.makeNetwork))
}
