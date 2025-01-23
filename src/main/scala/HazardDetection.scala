package rv32i.core

import chisel3._
import chisel3.util._

class HazardDetectionUnit extends Module {
  val io = IO(new Bundle {
    val rs1_decode = Input(UInt(5.W))
    val rs2_decode = Input(UInt(5.W))
    val rd_ex = Input(UInt(5.W))
    val mem_read_ex = Input(Bool())
    
    val stall_pc = Output(Bool())
    val stall_if_id = Output(Bool())
    val control_flush = Output(Bool())
  })
  
  // Default values
  io.stall_pc := false.B
  io.stall_if_id := false.B
  io.control_flush := false.B
  
  // Load-use hazard detection
  when(io.mem_read_ex && 
      ((io.rd_ex === io.rs1_decode) || (io.rd_ex === io.rs2_decode)) &&
      (io.rd_ex =/= 0.U)) {
    io.stall_pc := true.B
    io.stall_if_id := true.B
    io.control_flush := true.B
  }
}