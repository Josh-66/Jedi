package value

import context._
case class Chars(value:String) extends Addable with Ordered[Value]{
  def size():Exact ={
    Exact(value.length)
  }
  def subChars(from:Exact,to:Exact):Chars={
    Chars(value.substring(from.value,to.value))
  }
  def +(other:Value):Addable ={
    Chars(value+other.toString)
  }
  def compare(other:Value):Int ={
    other match {
      case x:Chars => value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }
  }

  override def toString: String = value
}
