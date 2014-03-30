import scala.util.parsing.combinator._
import scala.util.control.Exception._

case class Field(name: String)
case class Table(name: String)
case class Query(table: Table, fields: List[Field], terms: Option[List[Terms]])

trait Operation
object Select extends Operation
object From

trait Expr
case class StringExpr(value: String) extends Expr
case class NumericExpr(value: Int) extends Expr
case class Filter(key: String, value: Expr, operator: String) extends Expr
case class Term(filters: List[Filter]) extends Expr
case class Terms(terms: List[Term]) extends Expr

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

  def expr: SqlParser.Parser[Object] = {
    def _expr: SqlParser.Parser[Object] =
      repsep(literalValue~binaryOperator~literalValue, binaryOperator) ^^ { case xs =>
        xs.map { x => x match { case key~ope~value => Filter(key.toString, value, ope) }} 
      } | literalValue~binaryOperator~literalValue ^^ { case key~operator~value =>  value match {
          case StringExpr(v) => Filter(key.toString, value, operator)
          case NumericExpr(v) => Filter(key.toString, value, operator)
        }
      } 

// TODO:xsを処理
    "("~_expr~")"~rep(binaryOperator~expr) ^^ { case ("("~v~")"~xs) => v } |
    "("~_expr~")" ^^ { case "("~xs~")" => xs } |
    repsep(_expr, binaryOperator) ^^ { xs => xs.map { xs => xs match {
      case xs: List[_] => xs 
      case x => List(x)
    }}} |
    _expr
  }

  def query = operation~fields~from~table~opt(where) ^^ { 
    case operation~fields~from~table~where => where match {
      case Some(xs: List[List[Filter]]) => Query(table, fields, None)
      case Some(xs: List[Filter]) => Query(table, fields, None)
      case None => Query(table, fields, None)
    }
  }
  def parse(input: String): Option[Query] = {
    Option(parseAll(query, input).getOrElse(null))
  }
}
