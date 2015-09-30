package simulations

import org.scalatest.FunSuiteLike

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuiteLike {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or 4")

    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 5")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or 4")

    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 5")
  }

  test("demux example") {
    val in = new Wire
    val controls = List[Wire]()
    val outputs = new Wire :: Nil
    demux(in, controls, outputs)
    in.setSignal(false)
    run

    assert(outputs.map(_.getSignal) === List(false), "demux 1")

    in.setSignal(true)
    run
    
    assert(outputs.map(_.getSignal) === List(true), "demux 2")
  }

  test("demux example 2") {
    def testOut(actual: List[Wire], expected: List[Boolean], clue: String) =
      for ((w, v) <- (actual zip expected)) {
        assert(w.getSignal === v, clue)
      }

    val in = new Wire
    val controls = for (k <- (1 to 3).toList) yield new Wire
    val outputs = for (k <- (1 to math.pow(2, controls.size).toInt).toList) yield new Wire
    demux(in, controls, outputs)

    in.setSignal(false)
    for (c <- controls) {
      c.setSignal(true)
    }
    run
    assert(outputs.map(_.getSignal) === List(false, false, false, false, false, false, false, false), "demux 3")

    controls(0).setSignal(false)
    controls(1).setSignal(true)
    controls(2).setSignal(true)
    in.setSignal(false)
    run
    assert(outputs.map(_.getSignal) === List(false, false, false, false, false, false, false, false), "demux 4")
    in.setSignal(true)
    run
    assert(outputs.map(_.getSignal) === List(false, false, false, false, true, false, false, false), "demux 5")

    controls(0).setSignal(true)
    controls(1).setSignal(true)
    controls(2).setSignal(false)
    in.setSignal(false)
    run
    assert(outputs.map(_.getSignal) === List(false, false, false, false, false, false, false, false), "demux 6")
    in.setSignal(true)
    run
    testOut(outputs, List(false, true, false, false, false, false, false, false), "demux 7")
  }

}
