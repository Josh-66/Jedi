package expression
import context.Environment
import value.Value

case class Block(expressions:List[Expression]) extends SpecialForm{
  override def execute(env: Environment): Value = {
    var remaining = expressions
    val tempEnv = Environment(env)
    while (remaining.length>1) {
      remaining.head.execute(tempEnv)
      remaining=remaining.tail
    }
    remaining.head.execute(tempEnv)

  }
}
