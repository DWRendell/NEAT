import breeze.numerics.sigmoid

case class NeuralNetwork(
    nodes: List[Node],
    connections: List[Connection]
 ) {
  lazy val inputNodes =
    nodes.filterNot(node => connections.exists(_.to == node))
  lazy val outputNodes =
    nodes.filterNot(node => connections.exists(_.from == node))

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
    NeuralNetwork(nodes, connection :: connections)

  def splitConnection(connection: Connection, newNode: Node) = {
    assert(connections contains connection)
    val newConnection1 = Connection(connection.from, newNode, connection.weight)
    val newConnection2 = Connection(newNode, connection.to, connection.weight)
    NeuralNetwork(
      newNode :: nodes,
      newConnection1 :: newConnection2 :: connections.filterNot(_ == connection))
  }

  def replaceNode(oldNode: Node, newNode: Node) = {
    val newNodes = nodes.map {node =>
      if (node == oldNode) newNode
      else node
    }
    val newConnections = connections.map {
      case Connection(from, to, _) if from == oldNode => Connection(newNode, to)
    }.map {
      case Connection(from, to, _) if to == oldNode => Connection(from, newNode)
    }
    NeuralNetwork(newNodes, newConnections)
  }

  private def getNodesConnectedTo(node: Node) =
    connections
      .filter(_.to == node)
      .map(_.from)
  private def getNodesConnectedFrom(node: Node) =
    connections
      .filter(_.from == node)
      .map(_.to)

  def getConnectionDiagram = {
    nodes.map(i => nodes.map(j =>
      if (getNodesConnectedTo(i) contains j) "F"
      else if (getNodesConnectedFrom(i) contains j) "T"
      else " ").mkString(",")).mkString("\n")
  }
}

case class Node(id: Int) {
  def connectTo(to: Node) = Connection(this, to)
}

case class Connection(from: Node, to: Node, weight: Double = 1.0) {
  def withWeight(newWeight: Double) = Connection(from, to, newWeight)
}

object NeuralNetwork {
  val biasNode = Node(0)


}