package context

// import scala.util.parsing.combinator._
import expression.Identifier
import scala.util.parsing.combinator._
class JediException(gripe: String = "Jedi error ") extends Exception(gripe)
class UndefinedException(name: Identifier) extends JediException("Undefined identifier: " + name.name)
class TypeException(gripe: String = "Type Error") extends JediException(gripe)
class IllegalValueException(gripe: String = "Illegal Value") extends JediException(gripe)
class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")
