package FFT

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.math.{cos, sin, Pi, abs}

class IFFTSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "IFFT"

  // 輔助函數
  def generateTestSignal(n: Int = 8): Seq[(Double, Double)] = {
    val signal = Seq.tabulate(n) { i =>
      val x = 2.0 * Pi * i / n
      (0.1 * cos(x), 0.1 * sin(x))
    }
    println(s"\n生成的頻率分量 (n=$n):")
    signal.zipWithIndex.foreach { case ((real, imag), i) =>
      println(f"[$i] (實部: $real%.6f, 虛部: $imag%.6f)")
    }
    signal
  }

  def doubleToSInt(d: Double): SInt = {
    val scaled = (d * 8192).round.toInt  // 使用2^13作為縮放因子
    scaled.S(32.W)
  }

  def sIntToDouble(s: BigInt): Double = {
    s.toDouble / 8192.0
  }

  def calculateEnergy(signal: Seq[(Double, Double)]): Double = {
    signal.map { case (r, i) => r*r + i*i }.sum
  }

  def calculateError(signal1: Seq[(Double, Double)], signal2: Seq[(Double, Double)]): Double = {
    require(signal1.length == signal2.length, "Signals must have the same length")
    val pairs = signal1.zip(signal2)
    val errors = pairs.map { case ((r1, i1), (r2, i2)) =>
      val error = math.sqrt((r1-r2)*(r1-r2) + (i1-i2)*(i1-i2))
      val magnitude = math.sqrt(r1*r1 + i1*i1)
      if (magnitude > 1e-10) error/magnitude else error
    }
    errors.sum / errors.length
  }

  def waitForDone(dut: IFFT, maxCycles: Int = 100): Boolean = {
    var cycles = 0
    while (!dut.io.done.peek().litToBoolean && cycles < maxCycles) {
      dut.clock.step(1)
      cycles += 1
    }
    val isDone = dut.io.done.peek().litToBoolean
    println(s"\n完成於 $cycles 個週期")
    isDone
  }

  // 基本IFFT運算測試
  it should "執行8點IFFT運算" in {
    test(new IFFT(8)) { dut =>
      dut.io.mode.poke(false.B)  // 單獨IFFT測試時需要做/N
      val testSignal = generateTestSignal(8)

      println("\n設置輸入信號:")
      testSignal.zipWithIndex.foreach { case ((real, imag), i) =>
        dut.io.in(i).real.poke(doubleToSInt(real))
        dut.io.in(i).imag.poke(doubleToSInt(imag))
        println(f"輸入[$i]: ($real%.6f, $imag%.6f)")
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      assert(waitForDone(dut), "IFFT未能在預期時間內完成")

      println("\nIFFT結果:")
      val outputs = (0 until 8).map { i =>
        val real = sIntToDouble(dut.io.out(i).real.peek().litValue)
        val imag = sIntToDouble(dut.io.out(i).imag.peek().litValue)
        println(f"[$i] (實部: $real%.6f, 虛部: $imag%.6f)")
        (real, imag)
      }

      val inputEnergy = calculateEnergy(testSignal)
      val outputEnergy = calculateEnergy(outputs)
      println(f"\n輸入能量: $inputEnergy%.6f")
      println(f"輸出能量: $outputEnergy%.6f")
      val ratio = if (inputEnergy > 1e-10) outputEnergy/inputEnergy else 1.0
      println(f"能量比: $ratio%.4f\n")

      assert(ratio < 100 && ratio > 0.001,
             f"能量比例異常: $ratio%.4f")
    }
  }

  // 零輸入測試
  it should "正確處理零輸入" in {
    test(new IFFT(8)) { dut =>
      dut.io.mode.poke(false.B)
      println("\n設置零輸入...")

      for (i <- 0 until 8) {
        dut.io.in(i).real.poke(0.S)
        dut.io.in(i).imag.poke(0.S)
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      assert(waitForDone(dut), "零輸入處理未能在預期時間內完成")

      println("\n零輸入測試結果:")
      for (i <- 0 until 8) {
        val outReal = sIntToDouble(dut.io.out(i).real.peek().litValue)
        val outImag = sIntToDouble(dut.io.out(i).imag.peek().litValue)
        println(f"輸出[$i]: (實部: $outReal%.6f, 虛部: $outImag%.6f)")
        
        assert(math.abs(outReal) < 0.05 && math.abs(outImag) < 0.05,
          s"零輸入應產生接近零的輸出: idx=$i => ($outReal,$outImag)")
      }
    }
  }

  it should "在FFT-IFFT往返後恢復原始信號" in {
    test(new FFTPipeline) { dut =>
      val originalSignal = generateTestSignal(8)
      
      // 設置輸入
      println("\n設置輸入信號:")
      originalSignal.zipWithIndex.foreach { case ((real, imag), i) =>
        dut.io.in(i).real.poke(doubleToSInt(real))
        dut.io.in(i).imag.poke(doubleToSInt(imag))
        println(f"輸入[$i]: ($real%.6f, $imag%.6f)")
      }

      // 啟動處理
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // 等待完成
      var cycleCount = 0
      val maxCycles = 150
      while (!dut.io.done.peek().litToBoolean && cycleCount < maxCycles) {
        dut.clock.step(1)
        cycleCount += 1
      }
      
      assert(cycleCount < maxCycles, s"Pipeline未能在 $maxCycles 週期內完成")
      println(s"\n處理完成於 $cycleCount 週期")

      // 收集結果
      val reconstructed = (0 until 8).map { i =>
        val real = sIntToDouble(dut.io.out(i).real.peek().litValue)
        val imag = sIntToDouble(dut.io.out(i).imag.peek().litValue)
        (real, imag)
      }

      // 比較結果
      println("\n重建信號結果比較:")
      var maxError = 0.0
      reconstructed.zip(originalSignal).zipWithIndex.foreach {
        case (((rec_r, rec_i), (orig_r, orig_i)), i) =>
          val error = math.sqrt(math.pow(rec_r - orig_r, 2) + math.pow(rec_i - orig_i, 2))
          val magnitude = math.sqrt(orig_r * orig_r + orig_i * orig_i)
          val relativeError = if (magnitude > 1e-10) error / magnitude else error
          maxError = math.max(maxError, relativeError)
          
          println(f"[$i] 原始: ($orig_r%.6f, $orig_i%.6f)")
          println(f"    重建: ($rec_r%.6f, $rec_i%.6f)")
          println(f"    相對誤差: ${relativeError * 100}%.2f%%\n")
      }

      println(f"\n最大相對誤差: ${maxError * 100}%.2f%%")
      assert(maxError < 0.3, f"重建誤差過大: ${maxError * 100}%.2f%%")

      // 計算能量
      val inputEnergy = calculateEnergy(originalSignal)
      val outputEnergy = calculateEnergy(reconstructed)
      val energyRatio = outputEnergy / inputEnergy
      
      println(f"\n能量比較:")
      println(f"輸入能量: $inputEnergy%.6f")
      println(f"輸出能量: $outputEnergy%.6f")
      println(f"能量比: $energyRatio%.4f")
    }
  }

  // 直流信號測試
  it should "正確處理直流信號" in {
    test(new IFFT(8)) { dut =>
      dut.io.mode.poke(false.B) // 單獨IFFT需要做/N
      
      val dcValue = 0.1
      println(s"\n設置直流信號 (DC = $dcValue)...")
      
      for (i <- 0 until 8) {
        dut.io.in(i).real.poke(doubleToSInt(dcValue))
        dut.io.in(i).imag.poke(0.S)
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      assert(waitForDone(dut), "直流信號處理未能在預期時間內完成")

      println("\n直流信號測試結果:")
      var maxError = 0.0
      val expectedDcValue = dcValue / 8  // 因為做了/N，所以期望值要除以8
      
      for (i <- 0 until 8) {
        val outReal = sIntToDouble(dut.io.out(i).real.peek().litValue)
        val outImag = sIntToDouble(dut.io.out(i).imag.peek().litValue)
        println(f"輸出[$i]: (實部: $outReal%.6f, 虛部: $outImag%.6f)")
        
        val realError = math.abs(outReal - expectedDcValue)
        maxError = math.max(maxError, realError)
        
        assert(realError < 0.02 && math.abs(outImag) < 0.02,
          f"直流信號重建誤差過大: realErr=$realError%.6f, imagErr=${math.abs(outImag)}%.6f")
      }
      println(f"最大誤差: $maxError%.6f")
    }
  }
}