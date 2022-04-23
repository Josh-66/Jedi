package value

case class Notification(value:String) extends Value{
  override def toString: String = value
}
object Notification {
  val DONE = Notification("Done")
  val OK = Notification("Ok")
  val UNSPECIFIED = Notification("Unspecified")
}