package rv32i.core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ForwardingUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  "ForwardingUnit" should "work correctly" in {
    test(new ForwardingUnit) { dut =>
      // 測試各種前遞情境
      dut.io.rs1_ex.poke(5.U)
      dut.io.rs2_ex.poke(6.U)
      dut.io.rd_mem.poke(5.U)
      dut.io.reg_write_mem.poke(true.B)
      dut.clock.step()
      
      // 斷言前遞邏輯
      dut.io.forward_a.expect(1.U)
    }
  }
}