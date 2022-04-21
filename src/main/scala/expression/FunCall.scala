package expression
import value._
import context._
class FunCall(identifier:Identifier,args:List[Value]) extends Expression{
  def execute(env:Environment): Unit ={
    alu.execute(identifier,args)
  }
}
