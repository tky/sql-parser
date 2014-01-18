/**
 * sql :==  select fields from table
 * fields :== [a-zA-Z]+
 * table :== [a-zA-Z]+
 */

import scala.util.parsing.combinator._

case class Field(name: String)
case class Table(name: String)
case class Query(table: Table, fields: List[Field])

trait Operation
object Select extends Operation
object From

object SqlParser extends RegexParsers {
  def field = "[a-zA-Z*]+".r ^^ { f => Field(f) }
  def table = "[a-zA-Z]+".r ^^ { t => Table(t) }
  def fields = repsep(field, ",")
  def operation = "select" ^^ { _ => Select }
  def from = "from" ^^ { _ => From }
  def query = operation~fields~from~table ^^ { 
    case operation~fields~from~table => Query(table, fields)
  }
  def parse(input: String) = parseAll(query, input)
}
