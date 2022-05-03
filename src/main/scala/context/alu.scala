package context

import expression.*
import value.*

import scala.annotation.tailrec


object alu:
  def execute(opcode: Identifier, args: List[Value]): Value =
    opcode.name match
      case "add" => add(args)            // n-ary
      case "mul" => mul(args)            // n-ary
      case "sub" => sub(args)            // n-ary
      case "div" => div(args)            // n-ary
      case "less" => less(args)          // binary
      case "equals" => same(args)        // binary
      case "more" => more(args)          // binary
      case "unequals" => unequals(args)  // binary
      case "not" => not(args)            // unary
      case "write" => write(args)
      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
  // TBC


  private def add(args: List[Value]): Value = {
    @tailrec
    def helper(result: Addable, unseen: List[Value]): Addable =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: Addable => helper(result + h, unseen.tail)
        case _ => throw TypeException("Inputs to + must be addable")

    if (args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match
      case n: Addable => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to + must be addable")
  }
  private def mul(args: List[Value]): Value = {
    @tailrec
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result * h, unseen.tail)
        case _ => throw TypeException("Inputs to * must be numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match
      case n: value.Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to * must be numeric")
  }
  private def sub(args: List[Value]): Value = {
    @tailrec
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result - h, unseen.tail)
        case _ => throw TypeException("Inputs to - must be numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match
      case n: value.Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to - must be numeric")
  }
  private def div(args: List[Value]): Value = {
    @tailrec
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result / h, unseen.tail)
        case _ => throw TypeException("Inputs to / must be numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by /")
    args(0) match
      case n: value.Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to / must be numeric")
  }
  private def less(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by <")
    args(0) match
      case x: Ordered[Value] => Boole(x < args(1))
      case _ => throw TypeException("Inputs to < must be orderable")
  }
  private def same(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by ==")
    args(0) match
      case x: Ordered[Value] => Boole(x == args(1))
      case _ => throw TypeException("Inputs to == must be orderable")
  }
  private def more(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by >")
    args(0) match
      case x: Ordered[Value] => Boole(x > args(1))
      case _ => throw TypeException("Inputs to > must be orderable")
  }
  private def unequals(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by !=")
    args(0) match
      case x: Ordered[Value] => Boole(x != args(1))
      case _ => throw TypeException("Inputs to != must be orderable")
  }
  private def not(args: List[Value]): Value = {
    if (args.size != 1) throw new TypeException("1 inputs required by !")
    args(0) match
      case x: Boole => !x
      case _ => throw TypeException("Inputs to ! must be boole")
  }
  private def write(args: List[Value]): Value =
    println(args(0))
    Notification.DONE

  private def dereference(args: List[Value]): Value ={
    if (args.length!=1)
      throw new TypeException("Needs one argument for dereference")
    args(0) match {
      case v:Variable => v.value
      case _ => throw TypeException("Only Variables can be dereferenced")
    }
  }
  private def makeVar(args: List[Value]): Value ={
    if (args.length!=1)
      throw new TypeException("Needs one argument for makeVar")
    Variable(args(0))
  }


// etc.




