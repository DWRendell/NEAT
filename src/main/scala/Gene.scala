import scala.language.postfixOps

trait Gene {
  def transform(network: NeuralNetwork): NeuralNetwork
}

case class NewConnectionGene(from: Node, to: Node) extends Gene {
  def transform(network: NeuralNetwork) =
    network.withNewConnection(from connectTo to withRandomWeight)
}

case class SplitConnectionGene(connection: Connection, newNode: Node) extends Gene {
  def transform(network: NeuralNetwork) =
    network.splitConnection(connection, newNode)
}
