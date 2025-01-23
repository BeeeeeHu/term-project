FIRRTL version 1.1.0
circuit HazardDetectionUnit :
  module HazardDetectionUnit :
    input clock : Clock
    input reset : UInt<1>
    input io_rs1_decode : UInt<5>
    input io_rs2_decode : UInt<5>
    input io_rd_ex : UInt<5>
    input io_mem_read_ex : UInt<1>
    output io_stall_pc : UInt<1>
    output io_stall_if_id : UInt<1>
    output io_control_flush : UInt<1>

    node _T = eq(io_rd_ex, io_rs1_decode) @[HazardDetection.scala 25:18]
    node _T_1 = eq(io_rd_ex, io_rs2_decode) @[HazardDetection.scala 25:50]
    node _T_2 = or(_T, _T_1) @[HazardDetection.scala 25:37]
    node _T_3 = and(io_mem_read_ex, _T_2) @[HazardDetection.scala 24:23]
    node _T_4 = neq(io_rd_ex, UInt<1>("h0")) @[HazardDetection.scala 26:17]
    node _T_5 = and(_T_3, _T_4) @[HazardDetection.scala 25:70]
    node _GEN_0 = mux(_T_5, UInt<1>("h1"), UInt<1>("h0")) @[HazardDetection.scala 19:15 26:27 27:17]
    io_stall_pc <= _GEN_0
    io_stall_if_id <= _GEN_0
    io_control_flush <= _GEN_0
