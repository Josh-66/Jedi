package expression
import value._
import context._
case class FunCall(identifier:Identifier,args:List[Expression]) extends Expression{
  override def execute(env:Environment): Value ={
    if (env.contains(identifier)){
      env(identifier) match{
        case c:Closure => c(args.map(_.execute(env)))
        case _ =>alu.execute(identifier,args.map(_.execute(env)))
      }
    }
    else
      alu.execute(identifier,args.map(_.execute(env)))
  }
}
