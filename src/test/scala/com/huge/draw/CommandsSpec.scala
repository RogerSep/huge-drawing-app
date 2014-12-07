package com.huge.draw

import org.scalatest._
import org.scalatest.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class CreateCanvasCommandSpec extends FunSpec with Matchers {
  describe("CreateCanvasCommand") {
    describe("When non numeric parameters are passed") {
      it("should throw InvalidArguments if width or height are not integers") {
        forAll (Table(
          ("a",    "1"),
          ("10.1", "1"),
          ("",     "1"),
          ("1",    "a"),
          ("1",    "10.1"),
          ("1",    "")
        )) { (w, h) => 
          an [InvalidArguments] should be thrownBy {
            CreateCanvasExtractor.unapply(List("C", w, h))
          }
        }
      }
    }

    describe("When arguments are passed") {
      it("should throw InvalidArguments when too many or too few arguments are passed") {
        an [InvalidArguments] should be thrownBy {
          CreateCanvasExtractor.unapply(List("C"))
        }

        an [InvalidArguments] should be thrownBy {
          CreateCanvasExtractor.unapply(List("C", "10"))
        }
        
        an [InvalidArguments] should be thrownBy {
          CreateCanvasExtractor.unapply(List("C", "1", "2", "3"))
        }
      }

      it("should not parse 0 or negative width or height arguments") {
        forAll (Table(
          ("0", "0"),
          ("0", "1"),
          ("1", "0"),
          ("-1", "1"),
          ("1", "-1"),
          ("-1", "-1")
        )) { (w, h) =>
          an [InvalidArguments] should be thrownBy {
            CreateCanvasExtractor.unapply(List("C", w, h))
          }
        }
      }

      it("should not parse when the first argument is not C") {
        List("X", "1", "1") match {
          case CreateCanvasExtractor(c) => fail("C must be the first argument for correct parsing")
          case _ => ()
        }
      }

      it("should parse correctly valid commands") {
        List("C", "1", "2") match {
          case CreateCanvasExtractor(c) => (c.width, c.height) should equal (1, 2)
          case _ => fail("Could not parse CreateCanvasCommand")
        }
      }
    }

    describe("Creating canvases with the CreateCanvasCommand") {
      it("should create a 5x5 canvas") {
        Canvas()(CreateCanvasCommand(5, 5)).toString should equal (
          """-------
            _|     |
            _|     |
            _|     |
            _|     |
            _|     |
            _-------""".stripMargin('_'))
      }

      it("should create a 20x4 canvas") {
        Canvas()(CreateCanvasCommand(20, 4)).toString should equal (
          """----------------------
            _|                    |
            _|                    |
            _|                    |
            _|                    |
            _----------------------""".stripMargin('_'))
      }
    }
  }
}


class LineCommandSpec extends FunSpec with Matchers {
    describe("Line command") {
    describe("When non numeric parameters are passed") {
      it("should throw InvalidArguments if any arguments are not integers") {        
        forAll(Table(
          ("a", "2", "3", "4"),
          ("1", "a", "3", "4"),
          ("1", "2", "a", "4"),
          ("1", "2", "3", "a"),
          ("1.1", "2", "3", "4"),
          ("1", "2.2", "3", "4"),
          ("1", "2", "3.3", "4"),
          ("1", "2", "3", "4.4"),
          ("1", "2", "3", "")
        )) { (x1, y1, x2, y2) =>
          an [InvalidArguments] should be thrownBy {
            LineCommandExtractor.unapply(List("L", x1, y1, x2, y2))
          }
        }
      }
    }

    describe("When arguments are passed") {
      it("throw exception when too many or too few arguments are passed") {
        an [InvalidArguments] should be thrownBy {
          LineCommandExtractor.unapply(List("L"))
        }

        an [InvalidArguments] should be thrownBy {
          LineCommandExtractor.unapply(List("L", "10"))
        }
        
        an [InvalidArguments] should be thrownBy {
          LineCommandExtractor.unapply(List("L", "1", "2", "3"))
        }

        an [InvalidArguments] should be thrownBy {
          LineCommandExtractor.unapply(List("L", "1", "2", "3", "4", "5"))
        }
      }

      it("should throw when the line is not vertical or horizontal") {
        forAll(Table(
          ("1", "1", "2", "2"),
          ("1", "1", "2", "3")
        )) { (x1, y1, x2, y2) => 
          an[UnsupportedCommand] should be thrownBy {
            LineCommandExtractor.unapply(List("L", x1, y1, x2, y2))
          }
        }
      }

      it("should parse when arguments are correctly passed") {
        List("L", "1", "1", "1", "5") match {
          case LineCommandExtractor(l) => l should equal (LineCommand(1, 1, 1, 5))
        }
      }

      it("should not parse when the first argument is not L") {
        List("X", "1", "1", "1", "5") match {
          case LineCommandExtractor(l) => fail("L must be the first argument for correct parsing")
          case _ => ()
        }
      }
    }

    describe("When transforming a canvas") {
      val result = 
        """----------------------
          _|                    |
          _|xxxxxx              |
          _|                    |
          _|                    |
          _----------------------""".stripMargin('_')

      it("should draw a line when the line is within the canvas") {
        Canvas()(CreateCanvasCommand(20, 4))(LineCommand(1, 2, 6, 2)).toString should equal (result)
      }

      it("given two coordinates, should draw the same line") {
        forAll (Table(
          ("1", "2", "6", "2"),
          ("6", "2", "1", "2")
        )) { (x1, y1, x2, y2) =>
          Canvas()(CreateCanvasCommand(20, 4))(LineCommand(x1.toInt, y1.toInt, x2.toInt, y2.toInt)).toString should equal(result)
        }
      }

      it("should throw when a coordinate is out of the boundaries of the canvas.") {
        an [OutOfBounds] should be thrownBy {
          Canvas()(CreateCanvasCommand(20, 5))(LineCommand(1, 2, 21, 2))
        }
      }
    }
  }
}