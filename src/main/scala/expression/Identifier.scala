package expression
import context.Environment
import context.UndefinedException
case class Identifier(name: String) extends Expression {
  override def toString = name
  def execute(env: Environment) = {
    env(this)
  }
}