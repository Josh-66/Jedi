package context

import scala.util.parsing.combinator._
import expression._
import value._
import scala.collection.immutable.List.cons
/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
    case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
  }

  def  disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ more => Disjunction(con::more)
  }

  // conjunction ::= equality ~ ("&&" ~ equality)*
  def  conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case dis ~ Nil => dis
    case dis ~ more => Conjunction(dis::more)
  }
  // equality ::= inequality ~ ("==" ~ inequality)?
  def  equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case equ ~ Nil => equ
    case equ ~ Some(more) => FunCall(Identifier("equals"), equ :: more)
  }

  // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case ine ~ Nil => ine
    case ine ~ Some("<" ~ b) => FunCall(Identifier("less"),ine :: b)
    case ine ~ Some(">" ~ b) => FunCall(Identifier("more"),ine :: b)
    case ine ~ Some("!=" ~ b) => FunCall(Identifier("unequals"),ine :: b)
  }

  // sum ::= product ~ ("+" | "-") ~ product)*
  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product) ^^ {
    case p ~ more => parseSums(p, more)
  }

  // use tail recursion to imitate left reduce
  // parses a - b + c into add(sub(a, b), c)
  private def parseSums(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "+" ~ p => FunCall(Identifier("add"), List(exp, p))
        case "-" ~ p => FunCall(Identifier("sub"), List(exp, p))
      }
    if (unseen == Nil) result
    else parseSums(combiner(result, unseen.head), unseen.tail)
  }

  // product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term) ^^ {
    case p ~ more => parseProds(p, more)
  }
  private def parseProds(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "*" ~ p => FunCall(Identifier("mul"), List(exp, p))
        case "/" ~ p => FunCall(Identifier("div"), List(exp, p))
      }
    if (unseen == Nil) result
    else parseProds(combiner(result, unseen.head), unseen.tail)
  }

  def term: Parser[Expression]  = funCall | literal | "("~>expression<~")" ^^ {
    case t => t
  }

  def literal = boole | inexact | exact | chars | identifier

  // chars ::= any characters bracketed by quotes
  def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ {
    case characters => Chars(characters.substring(1, characters.length - 1))
  }

  // exact ::= 0|(\+|-)?[1-9][0-9]*
  def exact: Parser[Exact] = """0|(\+|-)?[1-9][0-9]*""".r^^{
    case ex => Exact(ex.toInt)
  }
  // inexact ::= (\+|-)?[0-9]+\.[0-9]+
  def inexact: Parser[Inexact] = """(\+|-)?[0-9]+\.[0-9]+""".r ^^{
    case iex => Inexact(iex.toDouble)
  }
  // boole ::= true|false
  def boole: Parser[Boole] = "true|false".r ^^ {
    case "true" => Boole.TRUE
    case "false" => Boole.FALSE
  }
  // identifier ::= [a-zA-Z][a-zA-Z0-9]*
  def identifier:Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^{
    case id => Identifier(id)
  }
  // funCall ::= identifier ~ operands
  def funCall:Parser[FunCall] = identifier ~ operands ^^{
    case id ~ op => FunCall(id,op)
  }
  // operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~> (expression ~ opt(rep("," ~> expression)) <~ ")" ^^{
    case exp ~ None => List(exp)
    case exp ~ Some(expL) => exp::expL
  }
}

