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
  }
}
