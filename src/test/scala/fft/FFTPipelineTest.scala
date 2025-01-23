package FFT

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import scala.math.{cos, sin, Pi}

class FFTPipelineSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FFTPipeline"

  def doubleToSInt(d: Double): SInt = {
    val scaled = (d * 8192).round.toInt
    scaled.S(32.W)
  }

  def sIntToDouble(s: BigInt): Double = {
    s.toDouble / 8192.0
  }

  def generateTestSignal(n: Int): Seq[(Double, Double)] = {
    Seq.tabulate(n) { i =>
      val x = 2.0 * Pi * i / n
      (0.1 * cos(x), 0.1 * sin(x))
    }
  }

  def waitForDone(dut: FFTPipeline, maxCycles: Int = 150): Boolean = {
    var cycles = 0
    while (!dut.io.done.peek().litToBoolean && cycles < maxCycles) {
      dut.clock.step(1)
      cycles += 1
    }
    val isDone = dut.io.done.peek().litToBoolean
    println(s"Pipeline ${if (isDone) "completed" else "timed out"} in $cycles cycles")
    isDone
  }

  it should "perform basic pipeline operation" in {
    test(new FFTPipeline) { dut =>
      val input = generateTestSignal(8)
      
      // Set input signal
      input.zipWithIndex.foreach { case ((real, imag), i) =>
        dut.io.in(i).real.poke(doubleToSInt(real))
        dut.io.in(i).imag.poke(doubleToSInt(imag))
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      assert(waitForDone(dut), "Processing should be complete")

      // Check results
      println("\nResults:")
      for (i <- 0 until 8) {
        val outReal = sIntToDouble(dut.io.out(i).real.peek().litValue)
        val outImag = sIntToDouble(dut.io.out(i).imag.peek().litValue)
        println(f"[$i] ($outReal%.6f, $outImag%.6f)")
      }
    }
  }

  it should "handle zero input correctly" in {
    test(new FFTPipeline) { dut =>
      // Set all inputs to zero
      for (i <- 0 until 8) {
        dut.io.in(i).real.poke(0.S)
        dut.io.in(i).imag.poke(0.S)
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      assert(waitForDone(dut), "Processing should complete")

      // Check if all outputs are zero
      for (i <- 0 until 8) {
        val outReal = sIntToDouble(dut.io.out(i).real.peek().litValue)
        val outImag = sIntToDouble(dut.io.out(i).imag.peek().litValue)
        assert(math.abs(outReal) < 0.01 && math.abs(outImag) < 0.01,
          s"Output[$i] should be near zero, got ($outReal, $outImag)")
      }
    }
  }

  it should "progress through pipeline stages correctly" in {
    test(new FFTPipeline) { dut =>
      val input = generateTestSignal(8)
      
      // Set input signal
      input.zipWithIndex.foreach { case ((real, imag), i) =>
        dut.io.in(i).real.poke(doubleToSInt(real))
        dut.io.in(i).imag.poke(doubleToSInt(imag))
      }

      // Start processing
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Let it run to completion
      assert(waitForDone(dut, 150), "Pipeline should complete within 150 cycles")

      // Verify completion
      assert(dut.io.done.peek().litToBoolean, "Pipeline should be done")
    }
  }
}