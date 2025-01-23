FIRRTL version 1.1.0
circuit ForwardingUnit :
  module ForwardingUnit :
    input clock : Clock
    input reset : UInt<1>
    input io_rs1_ex : UInt<5>
    input io_rs2_ex : UInt<5>
    input io_rd_mem : UInt<5>
    input io_rd_wb : UInt<5>
    input io_reg_write_mem : UInt<1>
    input io_reg_write_wb : UInt<1>
    output io_forward_a : UInt<2>
    output io_forward_b : UInt<2>

    node _T = neq(io_rd_mem, UInt<1>("h0")) @[ForwardingUnit.scala 23:39]
    node _T_1 = and(io_reg_write_mem, _T) @[ForwardingUnit.scala 23:25]
    node _T_2 = eq(io_rd_mem, io_rs1_ex) @[ForwardingUnit.scala 24:20]
    node _GEN_0 = mux(_T_2, UInt<1>("h1"), UInt<1>("h0")) @[ForwardingUnit.scala 19:16 24:35 25:20]
    node _T_3 = eq(io_rd_mem, io_rs2_ex) @[ForwardingUnit.scala 27:20]
    node _GEN_1 = mux(_T_3, UInt<1>("h1"), UInt<1>("h0")) @[ForwardingUnit.scala 20:16 27:35 28:20]
    node _GEN_2 = mux(_T_1, _GEN_0, UInt<1>("h0")) @[ForwardingUnit.scala 19:16 23:49]
    node _GEN_3 = mux(_T_1, _GEN_1, UInt<1>("h0")) @[ForwardingUnit.scala 20:16 23:49]
    node _T_4 = neq(io_rd_wb, UInt<1>("h0")) @[ForwardingUnit.scala 33:37]
    node _T_5 = and(io_reg_write_wb, _T_4) @[ForwardingUnit.scala 33:24]
    node _T_6 = neq(io_rd_mem, UInt<1>("h0")) @[ForwardingUnit.scala 34:41]
    node _T_7 = and(io_reg_write_mem, _T_6) @[ForwardingUnit.scala 34:27]
    node _T_8 = eq(io_rd_mem, io_rs1_ex) @[ForwardingUnit.scala 35:21]
    node _T_9 = and(_T_7, _T_8) @[ForwardingUnit.scala 34:50]
    node _T_10 = eq(_T_9, UInt<1>("h0")) @[ForwardingUnit.scala 34:8]
    node _T_11 = and(_T_5, _T_10) @[ForwardingUnit.scala 33:46]
    node _T_12 = eq(io_rd_wb, io_rs1_ex) @[ForwardingUnit.scala 36:19]
    node _GEN_4 = mux(_T_12, UInt<2>("h2"), _GEN_2) @[ForwardingUnit.scala 36:34 37:20]
    node _GEN_5 = mux(_T_11, _GEN_4, _GEN_2) @[ForwardingUnit.scala 35:38]
    node _T_13 = neq(io_rd_wb, UInt<1>("h0")) @[ForwardingUnit.scala 41:37]
    node _T_14 = and(io_reg_write_wb, _T_13) @[ForwardingUnit.scala 41:24]
    node _T_15 = neq(io_rd_mem, UInt<1>("h0")) @[ForwardingUnit.scala 42:41]
    node _T_16 = and(io_reg_write_mem, _T_15) @[ForwardingUnit.scala 42:27]
    node _T_17 = eq(io_rd_mem, io_rs2_ex) @[ForwardingUnit.scala 43:21]
    node _T_18 = and(_T_16, _T_17) @[ForwardingUnit.scala 42:50]
    node _T_19 = eq(_T_18, UInt<1>("h0")) @[ForwardingUnit.scala 42:8]
    node _T_20 = and(_T_14, _T_19) @[ForwardingUnit.scala 41:46]
    node _T_21 = eq(io_rd_wb, io_rs2_ex) @[ForwardingUnit.scala 44:19]
    node _GEN_6 = mux(_T_21, UInt<2>("h2"), _GEN_3) @[ForwardingUnit.scala 44:34 45:20]
    node _GEN_7 = mux(_T_20, _GEN_6, _GEN_3) @[ForwardingUnit.scala 43:38]
    io_forward_a <= _GEN_5
    io_forward_b <= _GEN_7
