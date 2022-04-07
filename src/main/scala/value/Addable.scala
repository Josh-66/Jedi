package value

trait Addable extends Value{
  def +(other:Value) : Addable
}
