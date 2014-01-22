import scala.util.parsing.combinator._
import scala.util.control.Exception._

case class Field(name: String)
case class Table(name: String)
case class Query(table: Table, fields: List[Field])
case class Term(key: String, expr: String, value: Any)

trait Operation
object Select extends Operation
object From

trait Expr
case class StringExpr(value: String) extends Expr
case class NumericExpr(value: Int) extends Expr
object DummyExpr extends Expr // <- こいつが無くなれば終了？

object SqlParser extends RegexParsers {
  def field = "[a-zA-Z*]+".r ^^ { f => Field(f) }
  def table = "[a-zA-Z]+".r ^^ { t => Table(t) }
  def fields = repsep(field, ",")
  def operation = "select" ^^ { _ => Select }
  def from = "from" ^^ { _ => From }
  def where = "where"~>expr

  def literalValue = numericLiteral | stringLiteral
  def numericLiteral = "[0-9]+".r ^^ { f =>  NumericExpr(f.toInt) }
  def stringLiteral = "[a-zA-Z_']+".r ^^ { f => StringExpr(f) }
  def nullLiteral = "null".r
  def currentTime = "CURRENT_TIME".r
  def currentDate = "CURRENT_DATE".r
  def currentTimeStamp = "CURRENT_TIMESTAMP".r
  def binaryOperator = "=|AND|OR".r

  def expr: SqlParser.Parser[Expr] = {
    def _expr: SqlParser.Parser[Expr] =
      repsep(literalValue~binaryOperator~literalValue, binaryOperator) ^^ { _ => DummyExpr }  |
      literalValue~binaryOperator~literalValue ^^ { _ => DummyExpr } 

    "("~_expr~")"~rep(binaryOperator~expr) ^^ { _ => DummyExpr } |
    "("~_expr~")" ^^ { _ => DummyExpr } |
    repsep(_expr, binaryOperator) ^^ { _ => DummyExpr } |
    _expr
  }

  def query = operation~fields~from~table~opt(where) ^^ { 
    case operation~fields~from~table~where => Query(table, fields)
  }
  def parse(input: String): Option[Query] = {
    println(parseAll(query, input))
    Option(parseAll(query, input).getOrElse(null))
  }
}
