package rv32i.core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class HazardDetectionTest extends AnyFlatSpec with ChiselScalatestTester {
  "HazardDetectionUnit" should "detect load-use hazard when rs1 matches" in {
    test(new HazardDetectionUnit) { dut =>
      // 模擬 rs1 匹配的載入使用hazard情境
      dut.io.rs1_decode.poke(5.U)
      dut.io.rs2_decode.poke(6.U)
      dut.io.rd_ex.poke(5.U)
      dut.io.mem_read_ex.poke(true.B)
      dut.clock.step()
      
      // 斷言hazard偵測
      dut.io.stall_pc.expect(true.B)
      dut.io.stall_if_id.expect(true.B)
      dut.io.control_flush.expect(true.B)
    }
  }

  it should "detect load-use hazard when rs2 matches" in {
    test(new HazardDetectionUnit) { dut =>
      // 模擬 rs2 匹配的載入使用hazard情境
      dut.io.rs1_decode.poke(6.U)
      dut.io.rs2_decode.poke(5.U)
      dut.io.rd_ex.poke(5.U)
      dut.io.mem_read_ex.poke(true.B)
      dut.clock.step()
      
      // 斷言hazard偵測
      dut.io.stall_pc.expect(true.B)
      dut.io.stall_if_id.expect(true.B)
      dut.io.control_flush.expect(true.B)
    }
  }

  it should "not detect hazard when no load-use condition exists" in {
    test(new HazardDetectionUnit) { dut =>
      // 模擬無hazard情境
      dut.io.rs1_decode.poke(1.U)
      dut.io.rs2_decode.poke(2.U)
      dut.io.rd_ex.poke(5.U)
      dut.io.mem_read_ex.poke(true.B)
      dut.clock.step()
      
      // 斷言無hazard
      dut.io.stall_pc.expect(false.B)
      dut.io.stall_if_id.expect(false.B)
      dut.io.control_flush.expect(false.B)
    }
  }

  it should "not detect hazard when mem_read_ex is false" in {
    test(new HazardDetectionUnit) { dut =>
      // 模擬非記憶體讀取情境
      dut.io.rs1_decode.poke(5.U)
      dut.io.rs2_decode.poke(6.U)
      dut.io.rd_ex.poke(5.U)
      dut.io.mem_read_ex.poke(false.B)
      dut.clock.step()
      
      // 斷言無hazard
      dut.io.stall_pc.expect(false.B)
      dut.io.stall_if_id.expect(false.B)
      dut.io.control_flush.expect(false.B)
    }
  }
}