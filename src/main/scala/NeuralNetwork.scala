import breeze.numerics.sigmoid

import scala.util.Random

case class NeuralNetwork(
    inputs: Int,
    outputs: Int,
    connections: List[Connection] = Nil
 ) {
  val inputNodes = (1 to inputs).map(Node).toList
  val outputNodes = (1 to outputs).map(i => Node(i + inputs)).toList

  def apply(inputValues: Double*): List[Double] = {
    assert(inputValues.length == inputNodes.length)

    var nodeValueCache: Map[Node, Double] = Map.empty

    def lazyEvaluateNode(node: Node) =
      nodeValueCache.get(node) match {
        case Some(value) => value
        case None =>
          val value = evaluateNode(node)
          nodeValueCache += (node -> value)
          value
      }

    def evaluateNode(node: Node): Double =
      node match {
        case Node(0) =>
          1.0
        case n if inputNodes contains n =>
          inputValues(inputNodes.indexOf(node))
        case _ =>
          sigmoid(connections
            .filter(_.to == node)
            .foldLeft(0.0) {(sum, connection) =>
            sum + connection.weight * evaluateNode(connection.from)
          })
      }

    outputNodes.map(lazyEvaluateNode)
  }

  def withNewConnection(connection: Connection) =
    NeuralNetwork(inputs, outputs, connection :: connections)

  def splitConnection(connection: Connection, newNode: Node) = {
    val oldConnection = connections.filter(c => c.from == connection.from && c.to == connection.to).head
    val newConnection1 = connection.from connectTo newNode withWeight 1.0
    val newConnection2 = newNode connectTo connection.to withWeight oldConnection.weight
    NeuralNetwork(
      inputs,
      outputs,
      newConnection1 :: newConnection2 :: connections.filterNot(_ == oldConnection))
  }

  override def toString = "Connections:\n" + connections.reverse.map(_.toString).mkString(",\n")
}

case class Node(id: Int) {
  def connectTo(to: Node) = Connection(this, to)
  override def toString = s"Node $id"
}

case class Connection(from: Node, to: Node, weight: Double = 1.0) {
  def withWeight(newWeight: Double) = Connection(from, to, newWeight)
  def withRandomWeight = withWeight(new Random().nextDouble())
  override def toString = s"$from is connected to $to with weight $weight}"
}

object NeuralNetwork {
  val biasNode = Node(0)
}