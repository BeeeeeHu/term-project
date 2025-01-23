import chisel3._
import chisel3.util._

class BExtensionIO extends Bundle {
  // Instruction input
  val instruction = Input(UInt(32.W))
  
  // Data inputs
  val rs1_data = Input(UInt(32.W))
  val rs2_data = Input(UInt(32.W))
  
  // Control signals
  val b_type = Output(Bool())
  val result = Output(UInt(32.W))
}

class BExtension extends Module {
  val io = IO(new BExtensionIO)
  
  // Default values
  io.b_type := false.B
  io.result := 0.U
  
  // Instruction decoding
  val opcode = io.instruction(6, 0)
  val funct3 = io.instruction(14, 12)
  val funct7 = io.instruction(31, 25)
  
  // B Extension instructions
  when(opcode === "b0110011".U && funct7 === "b0000000".U) {
    io.b_type := true.B
    
    switch(funct3) {
      // ANDN
      is("b111".U) {
        io.result := io.rs1_data & (~io.rs2_data)
      }
      // ORN
      is("b110".U) {
        io.result := io.rs1_data | (~io.rs2_data)
      }
      // XNOR
      is("b100".U) {
        io.result := ~(io.rs1_data ^ io.rs2_data)
      }
      // ROL
      is("b001".U) {
        io.result := (io.rs1_data << io.rs2_data(4,0)) | 
                     (io.rs1_data >> (32.U - io.rs2_data(4,0)))
      }
      // ROR
      is("b101".U) {
        io.result := (io.rs1_data >> io.rs2_data(4,0)) | 
                     (io.rs1_data << (32.U - io.rs2_data(4,0)))
      }
    }
  }
}