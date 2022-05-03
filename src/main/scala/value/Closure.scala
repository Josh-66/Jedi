package value

import context.Environment
import expression.Expression
import expression.Identifier

import scala.collection.mutable
case class Closure(parameters: List[Identifier], body:Expression,defEnv:Environment) extends Value {
  def apply(args:List[Value]): Value={
      val tempEnv = Environment(defEnv)
      tempEnv.bulkPut(parameters,args)
      body.execute(tempEnv)

  }
}
