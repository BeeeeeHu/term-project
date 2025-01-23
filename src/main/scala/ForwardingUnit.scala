package rv32i.core

import chisel3._
import chisel3.util._

class ForwardingUnit extends Module {
  val io = IO(new Bundle {
    val rs1_ex = Input(UInt(5.W))
    val rs2_ex = Input(UInt(5.W))
    val rd_mem = Input(UInt(5.W))
    val rd_wb = Input(UInt(5.W))
    val reg_write_mem = Input(Bool())
    val reg_write_wb = Input(Bool())
    val forward_a = Output(UInt(2.W))
    val forward_b = Output(UInt(2.W))
  })
  
  // Default: no forwarding
  io.forward_a := 0.U
  io.forward_b := 0.U
  
  // EX/MEM forwarding
  when(io.reg_write_mem && (io.rd_mem =/= 0.U)) {
    when(io.rd_mem === io.rs1_ex) {
      io.forward_a := 1.U
    }
    when(io.rd_mem === io.rs2_ex) {
      io.forward_b := 1.U
    }
  }
  
  // MEM/WB forwarding
  when(io.reg_write_wb && (io.rd_wb =/= 0.U) &&
       !(io.reg_write_mem && (io.rd_mem =/= 0.U) && 
         (io.rd_mem === io.rs1_ex))) {
    when(io.rd_wb === io.rs1_ex) {
      io.forward_a := 2.U
    }
  }
  
  when(io.reg_write_wb && (io.rd_wb =/= 0.U) &&
       !(io.reg_write_mem && (io.rd_mem =/= 0.U) && 
         (io.rd_mem === io.rs2_ex))) {
    when(io.rd_wb === io.rs2_ex) {
      io.forward_b := 2.U
    }
  }
}