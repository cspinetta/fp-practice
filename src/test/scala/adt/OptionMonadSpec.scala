package adt

import org.scalatest.{FunSpec, Matchers}

class OptionMonadSpec extends FunSpec with Matchers {

  describe("An OptionMonad") {
    describe("when has a value") {
      it("should be defined") {
        OptionMonad(1).isDefined shouldBe true
      }
      it("should return the value when invoke get") {
        OptionMonad(1).get shouldBe 1
      }
    }
  }
}
