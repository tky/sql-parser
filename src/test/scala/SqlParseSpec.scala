import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.matchers._

class SqlParserSpec extends FunSpec with Matchers {
  describe("SqlParse") {
    it("should parse simple query") {
      SqlParser.parse("select * from employ").isEmpty should be(false)
    }

    it("should parse where simple phrase") {
      SqlParser.parse("select * from employ where id = 1").isEmpty should be(false)
    }

    it("should parse mutl where phrase") {
      SqlParser.parse("select * from employ where id = 1 AND brach_id = 1").isEmpty should be(false)
      SqlParser.parse("select * from employ where id = 1 OR brach_id = 1").isEmpty should be(false)
    }

    it("should parse simple query covered wtih ()") {
      SqlParser.parse("select * from employ where (id = 1)").isEmpty should be(false)
      SqlParser.parse("select * from employ where (id = 1 AND brach_id = 1)").isEmpty should be(false)
      SqlParser.parse("select * from employ where (id = 1 OR brach_id = 1)").isEmpty should be(false)
    }

    it("should parse multi query covered wtih ()") {
      SqlParser.parse("select * from employ where (id = 1 AND brach_id = 1) AND (name = 'tk')").isEmpty should be(false)
      SqlParser.parse("select * from employ where (id = 1 AND brach_id = 1) OR (name = 'tk')").isEmpty should be(false)
      SqlParser.parse("select * from employ where (id = 1 AND brach_id = 1) OR (id = 2 AND name = 'tk')").isEmpty should be(false)
    }
  }
}
