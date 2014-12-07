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
    describe("When invalid parameters are passed") {
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

      it("should not parse 0 or negative arguments") {
        forAll (Table(
          ("0", "0", "0", "0"),
          ("0", "1", "1", "1"),
          ("1", "0", "1", "1"),
          ("-1", "1", "1", "1"),
          ("1", "-1", "1", "1"),
          ("-1", "-1", "1", "1")
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
      val blankCanvas = Canvas()(CreateCanvasCommand(20, 4))
      val result = 
        """----------------------
          _|                    |
          _|xxxxxx              |
          _|                    |
          _|                    |
          _----------------------""".stripMargin('_')

      it("should draw a line when the line is within the canvas") {
        blankCanvas(LineCommand(1, 2, 6, 2)).toString should equal (result)
      }

      it("given two coordinates, should draw the same line") {
        forAll (Table(
          ("1", "2", "6", "2"),
          ("6", "2", "1", "2")
        )) { (x1, y1, x2, y2) =>
          blankCanvas(LineCommand(x1.toInt, y1.toInt, x2.toInt, y2.toInt)).toString should equal(result)
        }
      }

      it("should throw when a coordinate is out of the boundaries of the canvas.") {
        an [OutOfBounds] should be thrownBy {
          blankCanvas(LineCommand(1, 2, 21, 2))
        }
      }
    }
  }
}

class RectangleCommandSpec extends FunSpec with Matchers {
  describe("Rectangle command") {
    describe("When invalid parameters are passed") {
      it("should throw InvalidArguments if any of them is not an integer") {        
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
            RectangleCommandExtractor.unapply(List("R", x1, y1, x2, y2))
          }
        }
      }

      it("should not parse 0 or negative arguments") {
        forAll (Table(
          ("0", "0", "0", "0"),
          ("0", "1", "1", "1"),
          ("1", "0", "1", "1"),
          ("-1", "1", "1", "1"),
          ("1", "-1", "1", "1"),
          ("-1", "-1", "1", "1")
        )) { (x1, y1, x2, y2) =>
          an [InvalidArguments] should be thrownBy {
            RectangleCommandExtractor.unapply(List("R", x1, y1, x2, y2))
          }
        }
      }
    }

    describe("When parameters are passed") {
      it("should throw when too many or too few arguments are passed") {
        an [InvalidArguments] should be thrownBy {
          RectangleCommandExtractor.unapply(List("R"))
        }

        an [InvalidArguments] should be thrownBy {
          RectangleCommandExtractor.unapply(List("R", "1", "2", "3"))
        }

        an [InvalidArguments] should be thrownBy {
          RectangleCommandExtractor.unapply(List("R", "1", "2", "3", "4", "5"))
        }
      }

      it("should not parse when the first argument is not an 'R") {
        List("X", "1", "1", "1", "1") match {
          case RectangleCommandExtractor(r) => fail("R must be the first argument for correct parsing")
          case _ => ()
        }
      }
    }

    describe("When transforming a canvas") {
      it("should throw when the rectangle is out of the boundaries of the canvas") {
        an [OutOfBounds] should be thrownBy {
          Canvas()(CreateCanvasCommand(1, 1))(RectangleCommand(1, 1, 2, 2))
        }
      }

      it("should draw a rectangle when the rectangle is in the canvas") {
        Canvas()(CreateCanvasCommand(20, 4))(RectangleCommand(16, 1, 20, 3)).toString should equal (
          """----------------------
            _|               xxxxx|
            _|               x   x|
            _|               xxxxx|
            _|                    |
            _----------------------""".stripMargin('_'))
      }
    }
  }
}

class BucketFillCommandSpec extends FunSpec with Matchers {
  describe("BucketFillCommand") {
    describe("When invalid parameters are passed") {
      it("should throw InvalidArguments if any of them is not an integer") {        
        forAll(Table(
          ("a", "2", "3"),
          ("1", "a", "3"),
          ("1.1", "2", "3"),
          ("1", "2.2", "3")
        )) { (x, y, colour) =>
          an [InvalidArguments] should be thrownBy {
            BucketFillCommandExtractor.unapply(List("B", x, y, colour))
          }
        }
      }

      it("fails if the colour is not one character long") {
        forAll(Table(
          ("1", "1", "too long"),
          ("1", "1", "")
        )) { (x, y, colour) =>
          an [UnsupportedCommand] should be thrownBy {
            BucketFillCommandExtractor.unapply(List("B", x, y, colour))
          }
        }
      }

      it("fails if there are too many or too few arguments") {
        an [InvalidArguments] should be thrownBy {
          BucketFillCommandExtractor.unapply(List("B"))
        }

        an [InvalidArguments] should be thrownBy {
          BucketFillCommandExtractor.unapply(List("B", "1"))
        }

        an [InvalidArguments] should be thrownBy {
          BucketFillCommandExtractor.unapply(List("B", "1", "2", "3", "4"))
        }
      }

      it("fails if the point is out of the boundaries of the canvas") {
        an [OutOfBounds] should be thrownBy {
          Canvas()(
            CreateCanvasCommand(4, 4),
            BucketFillCommand(5, 5, '_')
          )
        }
      }

      it("assumes white space when only the point is specified") {
        List("B", "1", "2") match {
          case BucketFillCommandExtractor(b) => b.colour should equal (' ')
          case _ => fail("It should assume whitespace when only coordinates are given")
        }
      }
    }

    describe("When transforming a canvas") {
      it("should paint the whole canvas when it's empty") {
        Canvas()(
          CreateCanvasCommand(20, 4), 
          BucketFillCommand(1, 1, '*')
        ).toString should equal (
          """----------------------
            _|********************|
            _|********************|
            _|********************|
            _|********************|
            _----------------------""".stripMargin('_'))
      }

      it("should paint a whole line when painting on it") {
        Canvas()(
            CreateCanvasCommand(20, 4),
            LineCommand(1, 1, 20, 1),
            BucketFillCommand(1, 1, '*')
          ).toString should equal (
          """----------------------
            _|********************|
            _|                    |
            _|                    |
            _|                    |
            _----------------------""".stripMargin('_'))
      }

      it("should paint the border of a rectangle") {
        Canvas()(
            CreateCanvasCommand(20, 4),
            RectangleCommand(1, 1, 5, 3),
            BucketFillCommand(1, 1, '*')
          ).toString should equal (
          """----------------------
            _|*****               |
            _|*   *               |
            _|*****               |
            _|                    |
            _----------------------""".stripMargin('_'))
      }

      it("should only fill the inside of a rectangle") {
        Canvas()(
            CreateCanvasCommand(20, 4),
            RectangleCommand(1, 1, 5, 3),
            BucketFillCommand(2, 2, '*')
          ).toString should equal (
          """----------------------
            _|xxxxx               |
            _|x***x               |
            _|xxxxx               |
            _|                    |
            _----------------------""".stripMargin('_'))
      }
    }
  }
}

class ComplexCommandSpec extends FlatSpec with Matchers {
  it should "handle provided test data ok" in {
    Canvas()(
      CreateCanvasCommand(20, 4),
      LineCommand(1, 2, 6, 2),
      LineCommand(6, 3, 6, 4),
      RectangleCommand(16, 1, 20, 3),
      BucketFillCommand(10, 3, 'o')
    ).toString should equal (
      """----------------------
        _|oooooooooooooooxxxxx|
        _|xxxxxxooooooooox   x|
        _|     xoooooooooxxxxx|
        _|     xoooooooooooooo|
        _----------------------""".stripMargin('_')
    )
  }
}