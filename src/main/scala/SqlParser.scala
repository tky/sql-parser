/**
 * sql :==  select fields from table where filters
 * fields :== [a-zA-Z]+
 * table :== [a-zA-Z]+
 * filters :== filter { "and" filter}
 * filter :== 
 * terms :== term { "and" term }
 * term :== [a-zA-Z] {"=" [a-zA-Z]}
 */

import scala.util.parsing.combinator._
import scala.util.control.Exception._

case class Field(name: String)
case class Table(name: String)
case class Query(table: Table, fields: List[Field], terms: Option[List[List[Term]]])
case class Term(key: String, expr: String, value: Any)

trait Operation
object Select extends Operation
object From


object SqlParser extends RegexParsers {
  def field = "[a-zA-Z*]+".r ^^ { f => Field(f) }
  def table = "[a-zA-Z]+".r ^^ { t => Table(t) }
  def fields = repsep(field, ",")
  def operation = "select" ^^ { _ => Select }
  def from = "from" ^^ { _ => From }
  def where = "where"~>terms

  def expr = "[=<>]+".r
  def termKey = "[a-zA-Z]+".r
  def termValue = "[a-zA-Z0-9']+".r
  def terms = term~rep("and"~>term) ^^ { case term~rest => List(term ++ rest.flatten) }
  def term = termKey~expr~termValue ^^ { case key~expr~value =>
    allCatch opt value.toInt match {
      case Some(v) => List(Term(key ,expr ,v))
      case None => List(Term(key ,expr ,value.replaceAll("'", "")))
    }
  }

  def query = operation~fields~from~table~opt(where) ^^ { 
    case operation~fields~from~table~where => Query(table, fields, where)
  }
  def parse(input: String): Option[Query] = Option(parseAll(query, input).getOrElse(null))
}
