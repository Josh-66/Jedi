package value

case class Variable(var value:Value) extends Value {
  override def toString: String = s"[$value]"
}
