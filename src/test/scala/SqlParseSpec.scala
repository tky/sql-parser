import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.matchers._

class SqlParserSpec extends FunSpec with Matchers {
  describe("SqlParse") {
    it("should parse simple query") {
      SqlParser.parse("select * from employ").isEmpty should be(false)
      val query = SqlParser.parse("select * from employ").get
      query.table should be(Table("employ"))
      query.fields should be(List(Field("*")))
    }
    it("should parse multi fields query") {
      val result = SqlParser.parse("select id, name from employ")
      result.isEmpty should be(false)

      val query = result.get
      query.table should be(Table("employ"))
      query.fields should be(List(Field("id"), Field("name")))
    }

    it("should parse simple where query") {
      val result = SqlParser.parse("select id, name from employ where name = 'tky' ")
      println(result)
      result.isEmpty should be(false)
      val query = result.get
      query.table should be(Table("employ"))
      query.fields should be(List(Field("id"), Field("name")))
      query.terms should be(Some(Term("name", "=",  "tky")))
    }
  }
}