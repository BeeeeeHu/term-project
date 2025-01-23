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

  for(i <- 0 until numPoints) {
    io.out(i).real := 0.S
    io.out(i).imag := 0.S
  }
  io.done := false.B

  private val logN = log2Ceil(numPoints)
  private val TWIDDLE_BP = 14
  
  def makeTwiddle(k: Int): (Int, Int) = {
    val angle = -2.0 * Pi * k / numPoints
    val scale = (1 << TWIDDLE_BP).toDouble
    val wr = (cos(angle) * scale).round.toInt
    val wi = (sin(angle) * scale).round.toInt
    (wr, wi)
  }

  val twiddleROM = VecInit((0 until numPoints/2).map { i =>
    val cpx = Wire(new ComplexNum)
    val (r, im) = makeTwiddle(i)
    cpx.real := r.S
    cpx.imag := im.S
    cpx
  })

  val dataRegs = Reg(Vec(numPoints, new ComplexNum))
  val idle :: bitRev :: compute :: doneState :: Nil = Enum(4)
  val state = RegInit(idle)
  
  val indexCnt = RegInit(0.U(log2Ceil(numPoints + 1).W))
  val stageCnt = RegInit(0.U(log2Ceil(logN + 1).W))
  val blockCnt = RegInit(0.U(log2Ceil(numPoints).W))
  val kCnt = RegInit(0.U(log2Ceil(numPoints).W))

  switch(state) {
    is(idle) {
      when(io.start) {
        indexCnt := 0.U
        stageCnt := 0.U
        blockCnt := 0.U
        kCnt := 0.U
        state := bitRev
      }
    }

    is(bitRev) {
      val i = indexCnt
      val r = bitReverse(i, logN)
      dataRegs(r).real := io.in(i).real
      dataRegs(r).imag := io.in(i).imag

      when(i === (numPoints-1).U) {
        indexCnt := 0.U
        stageCnt := 0.U
        blockCnt := 0.U
        kCnt := 0.U
        state := compute
      } .otherwise {
        indexCnt := i + 1.U
      }
    }

    is(compute) {
      when(stageCnt < logN.U) {
        val halfSize = 1.U << stageCnt
        val blockSize = halfSize << 1
        val numBlock = numPoints.U / blockSize

        val iIdx = blockCnt * blockSize + kCnt
        val jIdx = iIdx + halfSize

        val twStep = (numPoints.U / blockSize)
        val wIdx = kCnt * twStep
        val wCpx = twiddleROM(wIdx % (numPoints/2).U)

        val a = dataRegs(iIdx)
        val b = dataRegs(jIdx)

        val out1 = Wire(new ComplexNum)
        val out2 = Wire(new ComplexNum)

        when(stageCnt === (logN-1).U) {
          val results = butterflyWithScale(a, b, wCpx)
          out1.real := results._1.real
          out1.imag := results._1.imag
          out2.real := results._2.real
          out2.imag := results._2.imag
        }.otherwise {
          val results = butterflyNoScale(a, b, wCpx)
          out1.real := results._1.real
          out1.imag := results._1.imag
          out2.real := results._2.real
          out2.imag := results._2.imag
        }

        dataRegs(iIdx).real := out1.real
        dataRegs(iIdx).imag := out1.imag
        dataRegs(jIdx).real := out2.real
        dataRegs(jIdx).imag := out2.imag

        when(kCnt === (halfSize - 1.U)) {
          kCnt := 0.U
          when(blockCnt === (numBlock - 1.U)) {
            blockCnt := 0.U
            stageCnt := stageCnt + 1.U
          }.otherwise {
            blockCnt := blockCnt + 1.U
          }
        }.otherwise {
          kCnt := kCnt + 1.U
        }
      }.otherwise {
        state := doneState
      }
    }

    is(doneState) {
      io.done := true.B
      for(i <- 0 until numPoints) {
        io.out(i).real := dataRegs(i).real
        io.out(i).imag := dataRegs(i).imag
      }
      when(!io.start) {
        state := idle
      }
    }
  }

  def butterflyNoScale(a: ComplexNum, b: ComplexNum, w: ComplexNum): (ComplexNum, ComplexNum) = {
    val out1 = Wire(new ComplexNum)
    val out2 = Wire(new ComplexNum)

    val brwr = b.real * w.real
    val biwi = b.imag * w.imag
    val brwi = b.real * w.imag
    val biwr = b.imag * w.real

    val bwReal = ((brwr - biwi) >> TWIDDLE_BP).asSInt
    val bwImag = ((brwi + biwr) >> TWIDDLE_BP).asSInt

    out1.real := a.real + bwReal
    out1.imag := a.imag + bwImag
    out2.real := a.real - bwReal
    out2.imag := a.imag - bwImag

    (out1, out2)
  }

  def butterflyWithScale(a: ComplexNum, b: ComplexNum, w: ComplexNum): (ComplexNum, ComplexNum) = {
    val out1 = Wire(new ComplexNum)
    val out2 = Wire(new ComplexNum)

    val brwr = b.real * w.real
    val biwi = b.imag * w.imag
    val brwi = b.real * w.imag
    val biwr = b.imag * w.real

    val bwReal = ((brwr - biwi) >> TWIDDLE_BP).asSInt
    val bwImag = ((brwi + biwr) >> TWIDDLE_BP).asSInt

    val scaled1Real = (a.real + bwReal) >> logN
    val scaled1Imag = (a.imag + bwImag) >> logN
    val scaled2Real = (a.real - bwReal) >> logN
    val scaled2Imag = (a.imag - bwImag) >> logN

    out1.real := scaled1Real
    out1.imag := scaled1Imag
    out2.real := scaled2Real
    out2.imag := scaled2Imag

    (out1, out2)
  }

  def bitReverse(in: UInt, length: Int): UInt = {
    Cat((0 until length).map(i => in(i)).reverse)
  }
}