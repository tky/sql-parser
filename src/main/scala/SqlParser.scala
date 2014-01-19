/**
 * sql :==  select fields from table where terms
 * fields :== [a-zA-Z]+
 * table :== [a-zA-Z]+
 * terms :== [a-zA-Z] {"=" [a-zA-Z]}
 */

import scala.util.parsing.combinator._

case class Field(name: String)
case class Table(name: String)
case class Query(table: Table, fields: List[Field], terms: Option[Term])
case class Term(key: String, expr: String, value: Object)

trait Operation
object Select extends Operation
object From

object SqlParser extends RegexParsers {
  def field = "[a-zA-Z*]+".r ^^ { f => Field(f) }
  def table = "[a-zA-Z]+".r ^^ { t => Table(t) }
  def fields = repsep(field, ",")
  def operation = "select" ^^ { _ => Select }
  def from = "from" ^^ { _ => From }
  def where = "where"~>term

  def expr = "=".r
  def termKey = "[a-zA-Z]+".r
  def termValue = "[a-zA-Z']+".r
  def term = termKey~expr~termValue ^^ { case key~expr~value => Term(key ,expr ,value.replaceAll("'", "")) }

  def query = operation~fields~from~table~opt(where) ^^ { 
    case operation~fields~from~table~where => Query(table, fields, where)
  }
  def parse(input: String): Option[Query] = Option(parseAll(query, input).getOrElse(null))
}
