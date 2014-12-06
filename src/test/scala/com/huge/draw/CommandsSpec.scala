package com.huge.draw

import org.scalatest._
import org.scalatest.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class CommandsSpec extends FunSpec with Matchers {
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
            CreateCanvasCommand.unapply(List("C", w, h))
          }
        }
      }
    }

    describe("When arguments are passed") {
      it("should throw InvalidArguments when too many or too few areguments are passed") {
        an [InvalidArguments] should be thrownBy {
          CreateCanvasCommand.unapply(List("C"))
        }

        an [InvalidArguments] should be thrownBy {
          CreateCanvasCommand.unapply(List("C", "10"))
        }
        
        an [InvalidArguments] should be thrownBy {
          CreateCanvasCommand.unapply(List("C", "1", "2", "3"))
        }
      }

      it("should parse correctly valid commands") {
        List("C", "1", "2") match {
          case CreateCanvasCommand(w, h) => (w, h) should equal (1, 2)
          case _ => fail("Could not parse CreateCanvasCommand")
        }
      }
    }
  }
}
