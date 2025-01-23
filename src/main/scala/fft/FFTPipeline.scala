package FFT
import chisel3._
import chisel3.util._

class FFTPipeline extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done  = Output(Bool())
    val in    = Input(Vec(8, new ComplexNum()))
    val out   = Output(Vec(8, new ComplexNum()))
  })

  // 內部模組
  val fft  = Module(new FFT(8))
  val ifft = Module(new IFFT(8))

  // 狀態機定義
  val sIdle :: sFFT :: sIFFT :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  // 中間結果暫存器 (用來存 FFT 完的結果，接著再送到 IFFT)
  val intermediateResults = Reg(Vec(8, new ComplexNum))
  // 最終輸出暫存器
  val outputRegs = Reg(Vec(8, new ComplexNum))

  // ========== 預設值與 I/O 連線 ==========

  // FFT 預設
  fft.io.start := false.B
  fft.io.in := io.in  // 直接把外部 in 給 FFT

  // IFFT 預設
  ifft.io.start := false.B
  ifft.io.mode  := false.B  // 根據你的 IFFT 寫法，可切換是否做內部縮放

  // 先預設 IFFT 輸入為 0，接下來會在狀態機中做覆蓋
  for (i <- 0 until 8) {
    ifft.io.in(i).real := 0.S
    ifft.io.in(i).imag := 0.S
  }

  // Pipeline 輸出
  io.out  := outputRegs
  io.done := (state === sDone)

  // ========== 狀態機 ==========

  switch(state) {

    // ---- IDLE 狀態 ----
    is(sIdle) {
      when(io.start) {
        // 重置暫存器
        for (i <- 0 until 8) {
          intermediateResults(i).real := 0.S
          intermediateResults(i).imag := 0.S
          outputRegs(i).real := 0.S
          outputRegs(i).imag := 0.S
        }
        state := sFFT
      }
    }

    // ---- FFT 狀態 ----
    is(sFFT) {
      // 讓 FFT 開始
      fft.io.start := true.B

      // 等待 FFT 完成
      when(fft.io.done) {
        // 把 FFT 結果存進 intermediateResults
        for (i <- 0 until 8) {
          intermediateResults(i).real := fft.io.out(i).real
          intermediateResults(i).imag := fft.io.out(i).imag
        }
        // FFT 完後進入下一狀態做 IFFT
        state := sIFFT
      }
    }

    // ---- IFFT 狀態 ----
    is(sIFFT) {
      // 讓 IFFT 開始
      ifft.io.start := true.B
      // 此時把 intermediateResults 餵給 IFFT
      for (i <- 0 until 8) {
        ifft.io.in(i).real := intermediateResults(i).real
        ifft.io.in(i).imag := intermediateResults(i).imag
      }

      // 等待 IFFT 完成
      when(ifft.io.done) {
        // 把最終 IFFT 結果放到 outputRegs
        for (i <- 0 until 8) {
          outputRegs(i).real := ifft.io.out(i).real
          outputRegs(i).imag := ifft.io.out(i).imag
        }
        state := sDone
      }
    }

    // ---- DONE 狀態 ----
    is(sDone) {
      // 等待外部把 start 拉低，才能回到 Idle
      when(!io.start) {
        state := sIdle
      }
    }
  }
}
