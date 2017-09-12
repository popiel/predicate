package predicate

import org.scalatest._
import org.scalatest.prop._

class PredicateOpsSpec extends FunSpec with Matchers with PropertyChecks {
  import predicate.PredicateOps._

  describe("Predicate") {
    def isEven(x: Int): Boolean = x % 2 == 0
    def isSquare(x: Int): Boolean = {
      val root = Math.sqrt(x).toInt
      root * root == x
    }

    it("should be negatable") {
      forAll { (x: Int) =>
        (!(isEven _))(x) shouldBe (!(isEven(x)))
      }
    }
    it("should be conjuntable") {
      forAll { (x: Int) =>
        ((isEven _) & (isSquare _))(x) shouldBe (isEven(x) & isSquare(x))
      }
    }
  }

}
