package FFT

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.math.{cos, sin, Pi}

class FFTSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FFT"

  // 常數定義
  val ERROR_TOLERANCE = 0.01  // 統一的誤差容忍值
  val SIGNAL_AMPLITUDE = 0.1  // 統一的信號振幅
  val SCALE_FACTOR = 8192.0   // 統一的縮放因子

  // 生成測試信號
  def generateTestSignal(n: Int = 8): Seq[(Double, Double)] = {
    val signal = Seq.tabulate(n) { i =>
      val x = 2.0 * Pi * i / n
      (SIGNAL_AMPLITUDE * cos(x), SIGNAL_AMPLITUDE * sin(x))
    }
    println(s"\nGenerated test signal (n=$n):")
    signal.zipWithIndex.foreach { case ((real, imag), i) =>
      println(f"[$i] ($real%.6f, $imag%.6f)")
    }
    signal
  }

  // 定點數轉換
  def doubleToSInt(d: Double): SInt = {
    val scaled = (d * SCALE_FACTOR).round.toInt
    scaled.S(32.W)
  }

  // 驗證能量誤差
  def verifyEnergyError(inputEnergy: Double, outputEnergy: Double): Unit = {
    val error = math.abs(outputEnergy - inputEnergy)
    println(f"\nInput Energy: $inputEnergy%.6f")
    println(f"Output Energy: $outputEnergy%.6f")
    println(f"Energy Error: $error%.6f")
    
    assert(
      error < ERROR_TOLERANCE,
      s"Energy conservation error ($error) exceeds tolerance ($ERROR_TOLERANCE)"
    )
  }

  // 驗證幅度誤差
  def verifyMagnitudeError(expected: Double, actual: Double): Unit = {
    val error = math.abs(actual - expected)
    assert(
      error < ERROR_TOLERANCE,
      s"Magnitude error ($error) exceeds tolerance ($ERROR_TOLERANCE)"
    )
  }

  // 基本FFT測試
  it should "perform 8-point FFT operation" in {
    test(new FFT(8)) { dut =>
      val testSignal = generateTestSignal(8)
      
      // 設置輸入
      testSignal.zipWithIndex.foreach { case ((real, imag), i) =>
        dut.io.in(i).real.poke(doubleToSInt(real))
        dut.io.in(i).imag.poke(doubleToSInt(imag))
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      var cycleCount = 0
      while (!dut.io.done.peek().litToBoolean && cycleCount < 30) {
        dut.clock.step(1)
        cycleCount += 1
      }

      println(s"\nFFT completed in $cycleCount cycles")
      println("\nFFT results:")
      
      val outputs = (0 until 8).map { i =>
        val real = dut.io.out(i).real.peek().litValue.toDouble / SCALE_FACTOR
        val imag = dut.io.out(i).imag.peek().litValue.toDouble / SCALE_FACTOR
        println(f"[$i] ($real%.6f, $imag%.6f)")
        (real, imag)
      }
      
      val inputEnergy = testSignal.map { case (r, i) => r*r + i*i }.sum
      val outputEnergy = outputs.map { case (r, i) => r*r + i*i }.sum / 8
      verifyEnergyError(inputEnergy, outputEnergy)
    }
  }

  // 零輸入測試
  it should "handle zero input for 8-point FFT" in {
    test(new FFT(8)) { dut =>
      // 設置所有輸入為零
      for (i <- 0 until 8) {
        dut.io.in(i).real.poke(0.S)
        dut.io.in(i).imag.poke(0.S)
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      while (!dut.io.done.peek().litToBoolean) {
        dut.clock.step(1)
      }

      println("\nZero input test results:")
      for (i <- 0 until 8) {
        val outReal = dut.io.out(i).real.peek().litValue.toDouble / SCALE_FACTOR
        val outImag = dut.io.out(i).imag.peek().litValue.toDouble / SCALE_FACTOR
        println(f"Output[$i]: ($outReal%.6f, $outImag%.6f)")
        
        verifyMagnitudeError(0.0, math.sqrt(outReal*outReal + outImag*outImag))
      }
    }
  }

  // 脈衝響應測試
  it should "handle impulse input correctly" in {
    test(new FFT(8)) { dut =>
      val impulseAmplitude = SIGNAL_AMPLITUDE
      val expectedMagnitude = impulseAmplitude / 8  // 因為FFT會平均分配能量
      
      // 設置單位脈衝輸入
      for (i <- 0 until 8) {
        val real = if (i == 0) impulseAmplitude else 0.0
        val imag = 0.0
        dut.io.in(i).real.poke(doubleToSInt(real))
        dut.io.in(i).imag.poke(doubleToSInt(imag))
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      while (!dut.io.done.peek().litToBoolean) {
        dut.clock.step(1)
      }

      println("\nImpulse response:")
      for (i <- 0 until 8) {
        val outReal = dut.io.out(i).real.peek().litValue.toDouble / SCALE_FACTOR
        val outImag = dut.io.out(i).imag.peek().litValue.toDouble / SCALE_FACTOR
        println(f"Output[$i]: ($outReal%.6f, $outImag%.6f)")
        
        val magnitude = math.sqrt(outReal*outReal + outImag*outImag)
        verifyMagnitudeError(expectedMagnitude, magnitude)
      }
    }
  }
}