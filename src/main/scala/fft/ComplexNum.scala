package FFT

import chisel3._

class ComplexNum extends Bundle {
  val real = SInt(32.W)
  val imag = SInt(32.W)
}