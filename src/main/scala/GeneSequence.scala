case class GeneSequence(inputs: Int, outputs: Int, genes: List[Gene] = Nil) {
  def withGene(gene: Gene) = GeneSequence(inputs, outputs, genes :+ gene)

  def makeNetwork = genes.foldLeft(NeuralNetwork(inputs, outputs)) { (network, gene) =>
    gene.transform(network)
  }

  override def toString = s"Inputs: $inputs\n" + s"Outputs: $outputs\n" + genes.mkString(",\n")
}
