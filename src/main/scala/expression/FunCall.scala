package expression
import value._
import context._
case class FunCall(identifier:Identifier,args:List[Expression]) extends Expression{
  override def execute(env:Environment): Value ={
    alu.execute(identifier,args.map(_.execute(env)))
  }
}
