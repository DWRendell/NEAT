class Network(
    inputs: Int,
    outputs: Int,
    connections: List[Connection] = Nil
) {
  val nodes = connections.map(_.from) ::: connections.map(_.to) toSet
  val inputNodes =
    nodes.filter(_.isInstanceOf[InputNode]).toList.sortBy(_.id)
  val outputNodes =
    nodes.filter(_.isInstanceOf[OutputNode]).toList.sortBy(_.id)

  /**
   * Creates a new network the same as this one, but with the given connection
   * added.
   */
  //TODO: Check this leaves the network in a valid state
  def withConnection(connection: Connection) =
    new Network(inputs, outputs, connections :+ connection)

  /**
   * Creates a new network the same as this one, but with the given connection
   * removed if it is present.
   */
  //TODO: Check this leaves the network in a valid state
  def removeConnection(connection: Connection) =
    new Network(inputs, outputs, connections.filterNot(_.matches(connection)))

  /**
   * Creates a new network the same as this one, but with the given connection
   * removed and replaced with two new connections, with the given node in the
   * middle.
   * The first of the new connections will have weight 1.0, while the second
   * will match the split connection.
   */
  def splitConnection(connection: Connection, newNode: HiddenNode) =
    this
      .removeConnection(connection)
      .withConnection(
        new Connection(connection.from, newNode))
      .withConnection(
        new Connection(newNode, connection.to, connection.weight))

  /**
   * Returns the output when the given values are assigned to the input nodes
   * of the network
   */
  //TODO: Ensure network is valid before getting stuck in recursive loop
  def apply(inputValues: Double*): Seq[Double] = {
    require(inputValues.length == inputs,
      s"Wrong number of input values passed to network. " +
        s"Expected $inputs, received ${inputValues.length}")

    def valueAtNode(node: Node): Double = node match {
      case BiasNode =>
        1.0
      case InputNode(id) =>
        val index = inputNodes.indexOf(node)
        inputValues(index)
      case _ =>
        val connectionsToThisNode = connections.filter(_.to == node)
        connectionsToThisNode.foldLeft(0.0) { case (sum, connection) =>
            sum + connection.weight * valueAtNode(connection.from)
        }
    }
    outputNodes.map(valueAtNode)
  }

  override def toString =
    s"""
        |Inputs:  $inputs
        |Outputs: $outputs
        |Connections:
        |${connections.mkString("\n")}
      """.stripMargin
}
