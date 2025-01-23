package FFT

import chisel3._
import chisel3.util._
import scala.math.{Pi, cos, sin}

class FFT(val numPoints: Int) extends Module {
  require(isPow2(numPoints), s"FFT size must be a power of 2, got $numPoints")

  val io = IO(new Bundle {
    val in    = Input(Vec(numPoints, new ComplexNum))
    val out   = Output(Vec(numPoints, new ComplexNum))
    val start = Input(Bool())
    val done  = Output(Bool())
  })

  // Parameters
  private val logN = log2Ceil(numPoints)
  private val TWIDDLE_BP = 16  // Increased precision
  private val SCALE_FACTOR = (1 << TWIDDLE_BP).toDouble

  // Generate precise twiddle factors
  def makeTwiddle(k: Int): ComplexNum = {
    val angle = -2.0 * Pi * k / numPoints
    val wr = (cos(angle) * SCALE_FACTOR).round.toInt
    val wi = (sin(angle) * SCALE_FACTOR).round.toInt
    val cpx = Wire(new ComplexNum)
    cpx.real := wr.S
    cpx.imag := wi.S
    cpx
  }

  val twiddleROM = VecInit((0 until numPoints/2).map(makeTwiddle))

  // State machine and registers
  val idle :: bitRev :: compute :: normalize :: done :: Nil = Enum(5)
  val state = RegInit(idle)
  
  val dataRegs = Reg(Vec(numPoints, new ComplexNum))
  val indexCnt = RegInit(0.U(log2Ceil(numPoints + 1).W))
  val stageCnt = RegInit(0.U(log2Ceil(logN + 1).W))
  val blockCnt = RegInit(0.U(log2Ceil(numPoints).W))
  val kCnt     = RegInit(0.U(log2Ceil(numPoints).W))

  // Default outputs
  io.done := state === done
  io.out := dataRegs

  def roundShift(x: SInt, shift: UInt): SInt = {
    val mask = ((1.U << shift) - 1.U).asUInt
    val roundBit = (shift - 1.U)
    val round = (x > 0.S) && ((x.asUInt & mask) > (1.U << roundBit))
    val shifted = x >> shift
    Mux(round, shifted + 1.S, shifted)
  }

  switch(state) {
    is(idle) {
      when(io.start) {
        // Initialize data with prescaling
        for (i <- 0 until numPoints) {
          dataRegs(i).real := io.in(i).real << 3
          dataRegs(i).imag := io.in(i).imag << 3
        }
        indexCnt := 0.U
        stageCnt := 0.U
        blockCnt := 0.U
        kCnt := 0.U
        state := bitRev
      }
    }

    is(bitRev) {
      when(indexCnt < numPoints.U) {
        val i = indexCnt
        val r = bitReverse(i, logN)
        val temp = dataRegs(i)
        dataRegs(i) := dataRegs(r)
        dataRegs(r) := temp
        indexCnt := indexCnt + 1.U
      }.otherwise {
        state := compute
        indexCnt := 0.U
      }
    }

    is(compute) {
      when(stageCnt < logN.U) {
        val halfSize = 1.U << stageCnt
        val blockSize = halfSize << 1
        val iIdx = blockCnt * blockSize + kCnt
        val jIdx = iIdx + halfSize
        val twiddleIdx = (kCnt * (numPoints.U >> (stageCnt + 1.U))) % (numPoints.U / 2.U)
        
        val a = dataRegs(iIdx)
        val b = dataRegs(jIdx)
        val w = twiddleROM(twiddleIdx)
        
        val (out1, out2) = butterfly(a, b, w)
        
        // Controlled scaling during computation
        val shiftAmt = Mux(stageCnt === (logN-1).U, 2.U, 1.U)
        dataRegs(iIdx).real := roundShift(out1.real, shiftAmt)
        dataRegs(iIdx).imag := roundShift(out1.imag, shiftAmt)
        dataRegs(jIdx).real := roundShift(out2.real, shiftAmt)
        dataRegs(jIdx).imag := roundShift(out2.imag, shiftAmt)

        when(kCnt === (halfSize - 1.U)) {
          kCnt := 0.U
          when(blockCnt === ((numPoints.U / blockSize) - 1.U)) {
            blockCnt := 0.U
            stageCnt := stageCnt + 1.U
          }.otherwise {
            blockCnt := blockCnt + 1.U
          }
        }.otherwise {
          kCnt := kCnt + 1.U
        }
      }.otherwise {
        state := normalize
      }
    }

    is(normalize) {
      // Final normalization
      for (i <- 0 until numPoints) {
        dataRegs(i).real := roundShift(dataRegs(i).real, 1.U)
        dataRegs(i).imag := roundShift(dataRegs(i).imag, 1.U)
      }
      state := done
    }

    is(done) {
      when(!io.start) {
        state := idle
      }
    }
  }

  // Optimized butterfly computation
  def butterfly(a: ComplexNum, b: ComplexNum, w: ComplexNum): (ComplexNum, ComplexNum) = {
    val out1 = Wire(new ComplexNum)
    val out2 = Wire(new ComplexNum)
    
    // High precision complex multiplication for twiddle factor
    val bwr = (b.real * w.real) >> (TWIDDLE_BP - 1)  // Keep extra precision
    val bwi = (b.imag * w.imag) >> (TWIDDLE_BP - 1)
    val brwi = (b.real * w.imag) >> (TWIDDLE_BP - 1)
    val biwr = (b.imag * w.real) >> (TWIDDLE_BP - 1)

    val mulReal = bwr - bwi
    val mulImag = brwi + biwr

    // Butterfly addition/subtraction
    out1.real := (a.real +& mulReal).asSInt
    out1.imag := (a.imag +& mulImag).asSInt
    out2.real := (a.real -& mulReal).asSInt
    out2.imag := (a.imag -& mulImag).asSInt

    (out1, out2)
  }

  def bitReverse(in: UInt, width: Int): UInt = {
    Cat((0 until width).map(i => in(i)).reverse)
  }
}