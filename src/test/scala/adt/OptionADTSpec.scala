package adt

import org.scalatest.{FunSpec, Matchers}

class OptionADTSpec extends FunSpec with Matchers {

  describe("An OptionMonad") {
    describe("when has a value") {
      it("should be defined") {
        OptionADT(1).isDefined shouldBe true
      }
      it("should return the value when invoke get") {
        OptionADT(1).get shouldBe 1
      }
    }
  }
}
