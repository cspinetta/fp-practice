package parallel

import java.util.concurrent.{Executors, TimeUnit}

import org.scalatest.{FunSpec, Matchers}

class ParSpec extends FunSpec with Matchers {

  private val executor = Executors.newSingleThreadExecutor()

  describe("A Par") {
    describe("when call maxPar") {
      it("should return the max one") {
        val par = ParUtils.maxPar(List(1,2,3,4,5,6,7,8,9,10))
        val maxFut = Interpreter.run(executor)(par)
        val max = maxFut.get(1, TimeUnit.SECONDS)
        max shouldBe 10
      }
    }
  }
}
