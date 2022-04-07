package value

import context._

case class Boole(value: Boolean) extends Value{
  def &&(other:Value):Boole= {
    other match{
      case x:Boole => Boole(value && x.value)
      case _ => throw new TypeException("Boole operand required")
    }
  }
  def ||(other:Value):Boole= {
    other match{
      case x:Boole => Boole(value || x.value)
      case _ => throw new TypeException("Boole operand required")
    }
  }
  def unary_! : Boole= {
    Boole(!value)
  }
  override def toString:String = {
    value.toString
  }
}
case object Boole{
  val TRUE = Boole(true)
  val FALSE = Boole(false)
}
