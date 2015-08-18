import org.specs2.mutable.Specification

class NetworkSpec extends Specification {
  "Network" should {
    "be created" in {
      val network =
        new Network(inputs = 3, outputs = 1)
          .withConnection(Connection(InputNode(1), OutputNode(4)))
          .withConnection(Connection(InputNode(3), OutputNode(4)))
          .splitConnection(Connection(InputNode(1), OutputNode(4)), HiddenNode(5))
          .withConnection(Connection(InputNode(2), HiddenNode(5)))
      //println(network)
      network must beAnInstanceOf[Network]
    }

    "output values" in {
      val network = new Network(1, 1).withConnection(Connection(InputNode(1), OutputNode(2)))
      network(1.0) mustEqual Seq(1.0)
    }
  }
}
