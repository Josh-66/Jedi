package value

import context._

case class Inexact(value: Double) extends Numeric with Ordered[Value] :

  def +(other: Value): Addable =
    other match
      case x: Exact => Inexact(this.value + x.value.toDouble)
      case x: Inexact => Inexact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")

  def *(other: Value): Numeric =
    other match
      case x: Exact => Inexact(this.value * x.value.toDouble)
      case x: Inexact => Inexact(this.value * x.value)
      case _ => throw new TypeException("Numeric operand required")

  def -(other: Value): Numeric =
    other match
      case x: Exact => Inexact(this.value - x.value.toDouble)
      case x: Inexact => Inexact(this.value - x.value)
      case _ => throw new TypeException("Numeric operand required")

  def /(other: Value): Numeric =
    other match
      case x: Exact =>
        if (x.value == 0) throw IllegalValueException("Can't divide by 0")
        else Inexact(this.value / x.value.toDouble)
      case x: Inexact =>
        if (x.value == 0.0) throw IllegalValueException("Can't divide by 0")
        else Inexact(this.value / x.value)
      case _ => throw new TypeException("Numeric operand required")

  def unary_- : Inexact = Inexact(-this.value)

  def compare(other: Value): Int =
    other match
      case x: Exact => this.value.compare(x.value.toDouble)
      case x: Inexact => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")

  override def equals(other:Any): Boolean ={
    other match
      case x: Exact => this.value == x.value.toDouble
      case x: Inexact => this.value == x.value
      case _ => throw new TypeException("Arguments must be comparable")
  }
  override def toString = this.value.toString
