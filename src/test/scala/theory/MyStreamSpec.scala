package theory

import java.util.concurrent.Executors

import org.scalatest.{FunSpec, Matchers}

class MyStreamSpec extends FunSpec with Matchers {
  import MyStreamExamples.StreamTransducer._

  private val executor = Executors.newSingleThreadExecutor()

  describe("A Stream processor") {
    it("should process a stream with a lift function given") {
      Process.lift((i: Int) => i * 3)(Stream(2,5,10,-4,-2,7))
        .toList should be(List(6,15,30,-12,-6,21))
    }
    it("should sum all values") {
      Process.sum(Stream(10,333,1,2)).toList should be(List(10,343,344,346))
    }
    it("should sum all values (via loop())") {
      Process.sumViaLoop(Stream(10,333,1,2)).toList should be(List(10,343,344,346))
    }
    it("should take N values") {
      Process.take(3)(Stream(10,333,1,2,3,6,4,2,4)).toList should be(List(10,333,1))
    }
    it("should take values while a condition") {
      Process.takeWhile((i: Int) => i >= 10)(Stream(10,333,19,2,3,6,4,2,4,35,4))
        .toList should be(List(10,333,19))
    }
    it("should take 0 values when N is negative") {
      Process.take(-3)(Stream(10,333,1,2,3,6,4,2,4)).toList should be(List())
    }
    it("should drop N values") {
      Process.drop(3)(Stream(10,333,1,2,3,6,4,2,4)).toList should be(List(2,3,6,4,2,4))
    }
    it("should drop values while a condition") {
      Process.dropWhile((i: Int) => i >= 10)(Stream(10,333,19,2,3,6,4,2,4,35,4))
        .toList should be(List(2,3,6,4,2,4,35,4))
    }
    it("should drop 0 values when N is negative") {
      Process.drop(-3)(Stream(10,333,1,2,3,6,4,2,4)).toList should be(List(10,333,1,2,3,6,4,2,4))
    }
    it("should count each value processed") {
      Process.count(Stream(10,333,1,2,3,6,4,2,4)).toList should be(List(1,2,3,4,5,6,7,8,9))
    }
    it("should count each value processed (via loop())") {
      Process.countViaLoop(Stream(10,333,1,2,3,6,4,2,4)).toList should be(List(1,2,3,4,5,6,7,8,9))
    }
    it("should calculate the average of the values seen so far") {
      Process.mean(Stream(10,333,2,3)).toList should be(List(10, 171.5, 115, 87))
    }
    it("should calculate the average of the values seen so far (via loop() abstraction)") {
      Process.meanViaLoop(Stream(10,333,2,3)).toList should be(List(10, 171.5, 115, 87))
    }
  }
}
