case class Connection(from: Node, to: Node, weight: Double = 1.0) {
  def matches(otherConnection: Connection) =
    from == otherConnection.from && to == otherConnection.to

  override def toString = s"$from -> $to (weight $weight)"
}
