trait Node {
  val id: Int

  override def toString = s"Node $id"
}

case class InputNode(id: Int) extends Node {
  override def toString = super.toString + " (in)"
}

case class OutputNode(id: Int) extends Node {
  override def toString = super.toString + " (out)"
}

case class HiddenNode(id: Int) extends Node

case object BiasNode extends Node {
  val id = 0
  override def toString = "Bias Node"
}
