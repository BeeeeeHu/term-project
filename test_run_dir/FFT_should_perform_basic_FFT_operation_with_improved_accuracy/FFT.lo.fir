FIRRTL version 1.1.0
circuit FFT :
  module FFT :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_real : SInt<32>
    input io_in_0_imag : SInt<32>
    input io_in_1_real : SInt<32>
    input io_in_1_imag : SInt<32>
    input io_in_2_real : SInt<32>
    input io_in_2_imag : SInt<32>
    input io_in_3_real : SInt<32>
    input io_in_3_imag : SInt<32>
    input io_in_4_real : SInt<32>
    input io_in_4_imag : SInt<32>
    input io_in_5_real : SInt<32>
    input io_in_5_imag : SInt<32>
    input io_in_6_real : SInt<32>
    input io_in_6_imag : SInt<32>
    input io_in_7_real : SInt<32>
    input io_in_7_imag : SInt<32>
    output io_out_0_real : SInt<32>
    output io_out_0_imag : SInt<32>
    output io_out_1_real : SInt<32>
    output io_out_1_imag : SInt<32>
    output io_out_2_real : SInt<32>
    output io_out_2_imag : SInt<32>
    output io_out_3_real : SInt<32>
    output io_out_3_imag : SInt<32>
    output io_out_4_real : SInt<32>
    output io_out_4_imag : SInt<32>
    output io_out_5_real : SInt<32>
    output io_out_5_imag : SInt<32>
    output io_out_6_real : SInt<32>
    output io_out_6_imag : SInt<32>
    output io_out_7_real : SInt<32>
    output io_out_7_imag : SInt<32>
    input io_start : UInt<1>
    output io_done : UInt<1>

    node _dataRegs_T = asUInt(asSInt(UInt<32>("h0"))) @[FFT.scala 73:72]
    node _dataRegs_WIRE_1 = pad(_dataRegs_T, 64) @[FFT.scala 73:{72,72}]
    node _dataRegs_T_1 = bits(_dataRegs_WIRE_1, 31, 0) @[FFT.scala 73:72]
    node _dataRegs_T_2 = asSInt(_dataRegs_T_1) @[FFT.scala 73:72]
    node _dataRegs_T_3 = bits(_dataRegs_WIRE_1, 63, 32) @[FFT.scala 73:72]
    node _dataRegs_T_4 = asSInt(_dataRegs_T_3) @[FFT.scala 73:72]
    node _dataRegs_T_5 = asUInt(asSInt(UInt<32>("h0"))) @[FFT.scala 73:72]
    node _dataRegs_WIRE_3 = pad(_dataRegs_T_5, 64) @[FFT.scala 73:{72,72}]
    node _dataRegs_T_6 = bits(_dataRegs_WIRE_3, 31, 0) @[FFT.scala 73:72]
    node _dataRegs_T_7 = asSInt(_dataRegs_T_6) @[FFT.scala 73:72]
    node _dataRegs_T_8 = bits(_dataRegs_WIRE_3, 63, 32) @[FFT.scala 73:72]
    node _dataRegs_T_9 = asSInt(_dataRegs_T_8) @[FFT.scala 73:72]
    node _dataRegs_T_10 = asUInt(asSInt(UInt<32>("h0"))) @[FFT.scala 73:72]
    node _dataRegs_WIRE_5 = pad(_dataRegs_T_10, 64) @[FFT.scala 73:{72,72}]
    node _dataRegs_T_11 = bits(_dataRegs_WIRE_5, 31, 0) @[FFT.scala 73:72]
    node _dataRegs_T_12 = asSInt(_dataRegs_T_11) @[FFT.scala 73:72]
    node _dataRegs_T_13 = bits(_dataRegs_WIRE_5, 63, 32) @[FFT.scala 73:72]
    node _dataRegs_T_14 = asSInt(_dataRegs_T_13) @[FFT.scala 73:72]
    node _dataRegs_T_15 = asUInt(asSInt(UInt<32>("h0"))) @[FFT.scala 73:72]
    node _dataRegs_WIRE_7 = pad(_dataRegs_T_15, 64) @[FFT.scala 73:{72,72}]
    node _dataRegs_T_16 = bits(_dataRegs_WIRE_7, 31, 0) @[FFT.scala 73:72]
    node _dataRegs_T_17 = asSInt(_dataRegs_T_16) @[FFT.scala 73:72]
    node _dataRegs_T_18 = bits(_dataRegs_WIRE_7, 63, 32) @[FFT.scala 73:72]
    node _dataRegs_T_19 = asSInt(_dataRegs_T_18) @[FFT.scala 73:72]
    node _dataRegs_T_20 = asUInt(asSInt(UInt<32>("h0"))) @[FFT.scala 73:72]
    node _dataRegs_WIRE_9 = pad(_dataRegs_T_20, 64) @[FFT.scala 73:{72,72}]
    node _dataRegs_T_21 = bits(_dataRegs_WIRE_9, 31, 0) @[FFT.scala 73:72]
    node _dataRegs_T_22 = asSInt(_dataRegs_T_21) @[FFT.scala 73:72]
    node _dataRegs_T_23 = bits(_dataRegs_WIRE_9, 63, 32) @[FFT.scala 73:72]
    node _dataRegs_T_24 = asSInt(_dataRegs_T_23) @[FFT.scala 73:72]
    node _dataRegs_T_25 = asUInt(asSInt(UInt<32>("h0"))) @[FFT.scala 73:72]
    node _dataRegs_WIRE_11 = pad(_dataRegs_T_25, 64) @[FFT.scala 73:{72,72}]
    node _dataRegs_T_26 = bits(_dataRegs_WIRE_11, 31, 0) @[FFT.scala 73:72]
    node _dataRegs_T_27 = asSInt(_dataRegs_T_26) @[FFT.scala 73:72]
    node _dataRegs_T_28 = bits(_dataRegs_WIRE_11, 63, 32) @[FFT.scala 73:72]
    node _dataRegs_T_29 = asSInt(_dataRegs_T_28) @[FFT.scala 73:72]
    node _dataRegs_T_30 = asUInt(asSInt(UInt<32>("h0"))) @[FFT.scala 73:72]
    node _dataRegs_WIRE_13 = pad(_dataRegs_T_30, 64) @[FFT.scala 73:{72,72}]
    node _dataRegs_T_31 = bits(_dataRegs_WIRE_13, 31, 0) @[FFT.scala 73:72]
    node _dataRegs_T_32 = asSInt(_dataRegs_T_31) @[FFT.scala 73:72]
    node _dataRegs_T_33 = bits(_dataRegs_WIRE_13, 63, 32) @[FFT.scala 73:72]
    node _dataRegs_T_34 = asSInt(_dataRegs_T_33) @[FFT.scala 73:72]
    node _dataRegs_T_35 = asUInt(asSInt(UInt<32>("h0"))) @[FFT.scala 73:72]
    node _dataRegs_WIRE_15 = pad(_dataRegs_T_35, 64) @[FFT.scala 73:{72,72}]
    node _dataRegs_T_36 = bits(_dataRegs_WIRE_15, 31, 0) @[FFT.scala 73:72]
    node _dataRegs_T_37 = asSInt(_dataRegs_T_36) @[FFT.scala 73:72]
    node _dataRegs_T_38 = bits(_dataRegs_WIRE_15, 63, 32) @[FFT.scala 73:72]
    node _dataRegs_T_39 = asSInt(_dataRegs_T_38) @[FFT.scala 73:72]
    reg dataRegs_0_real : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_0_real) @[FFT.scala 73:25]
    reg dataRegs_0_imag : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_0_imag) @[FFT.scala 73:25]
    reg dataRegs_1_real : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_1_real) @[FFT.scala 73:25]
    reg dataRegs_1_imag : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_1_imag) @[FFT.scala 73:25]
    reg dataRegs_2_real : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_2_real) @[FFT.scala 73:25]
    reg dataRegs_2_imag : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_2_imag) @[FFT.scala 73:25]
    reg dataRegs_3_real : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_3_real) @[FFT.scala 73:25]
    reg dataRegs_3_imag : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_3_imag) @[FFT.scala 73:25]
    reg dataRegs_4_real : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_4_real) @[FFT.scala 73:25]
    reg dataRegs_4_imag : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_4_imag) @[FFT.scala 73:25]
    reg dataRegs_5_real : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_5_real) @[FFT.scala 73:25]
    reg dataRegs_5_imag : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_5_imag) @[FFT.scala 73:25]
    reg dataRegs_6_real : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_6_real) @[FFT.scala 73:25]
    reg dataRegs_6_imag : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_6_imag) @[FFT.scala 73:25]
    reg dataRegs_7_real : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_7_real) @[FFT.scala 73:25]
    reg dataRegs_7_imag : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataRegs_7_imag) @[FFT.scala 73:25]
    reg stageCounter : UInt<2>, clock with :
      reset => (UInt<1>("h0"), stageCounter) @[FFT.scala 74:29]
    reg groupCounter : UInt<3>, clock with :
      reset => (UInt<1>("h0"), groupCounter) @[FFT.scala 75:29]
    reg butterflyCounter : UInt<2>, clock with :
      reset => (UInt<1>("h0"), butterflyCounter) @[FFT.scala 76:33]
    reg state : UInt<2>, clock with :
      reset => (UInt<1>("h0"), state) @[FFT.scala 79:22]
    node _io_done_T = eq(state, UInt<2>("h2")) @[FFT.scala 81:20]
    node _T = eq(UInt<2>("h0"), state) @[FFT.scala 84:17]
    node _GEN_0 = mux(io_start, io_in_0_real, dataRegs_0_real) @[FFT.scala 86:22 73:25 89:28]
    node _GEN_1 = mux(io_start, io_in_0_imag, dataRegs_0_imag) @[FFT.scala 86:22 73:25 90:28]
    node _GEN_2 = mux(io_start, io_in_1_real, dataRegs_1_real) @[FFT.scala 86:22 73:25 89:28]
    node _GEN_3 = mux(io_start, io_in_1_imag, dataRegs_1_imag) @[FFT.scala 86:22 73:25 90:28]
    node _GEN_4 = mux(io_start, io_in_2_real, dataRegs_2_real) @[FFT.scala 86:22 73:25 89:28]
    node _GEN_5 = mux(io_start, io_in_2_imag, dataRegs_2_imag) @[FFT.scala 86:22 73:25 90:28]
    node _GEN_6 = mux(io_start, io_in_3_real, dataRegs_3_real) @[FFT.scala 86:22 73:25 89:28]
    node _GEN_7 = mux(io_start, io_in_3_imag, dataRegs_3_imag) @[FFT.scala 86:22 73:25 90:28]
    node _GEN_8 = mux(io_start, io_in_4_real, dataRegs_4_real) @[FFT.scala 86:22 73:25 89:28]
    node _GEN_9 = mux(io_start, io_in_4_imag, dataRegs_4_imag) @[FFT.scala 86:22 73:25 90:28]
    node _GEN_10 = mux(io_start, io_in_5_real, dataRegs_5_real) @[FFT.scala 86:22 73:25 89:28]
    node _GEN_11 = mux(io_start, io_in_5_imag, dataRegs_5_imag) @[FFT.scala 86:22 73:25 90:28]
    node _GEN_12 = mux(io_start, io_in_6_real, dataRegs_6_real) @[FFT.scala 86:22 73:25 89:28]
    node _GEN_13 = mux(io_start, io_in_6_imag, dataRegs_6_imag) @[FFT.scala 86:22 73:25 90:28]
    node _GEN_14 = mux(io_start, io_in_7_real, dataRegs_7_real) @[FFT.scala 86:22 73:25 89:28]
    node _GEN_15 = mux(io_start, io_in_7_imag, dataRegs_7_imag) @[FFT.scala 86:22 73:25 90:28]
    node _GEN_16 = mux(io_start, UInt<1>("h0"), stageCounter) @[FFT.scala 86:22 92:22 74:29]
    node _GEN_17 = mux(io_start, UInt<1>("h0"), groupCounter) @[FFT.scala 86:22 93:22 75:29]
    node _GEN_18 = mux(io_start, UInt<1>("h0"), butterflyCounter) @[FFT.scala 86:22 94:26 76:33]
    node _GEN_19 = mux(io_start, UInt<2>("h1"), state) @[FFT.scala 86:22 95:15 79:22]
    node _T_1 = eq(UInt<2>("h1"), state) @[FFT.scala 84:17]
    node _T_2 = lt(stageCounter, UInt<2>("h3")) @[FFT.scala 100:25]
    node stageSize = dshl(UInt<1>("h1"), stageCounter) @[FFT.scala 101:30]
    node groupSize = dshl(stageSize, UInt<1>("h1")) @[FFT.scala 102:36]
    node groupOffset = mul(groupCounter, groupSize) @[FFT.scala 104:40]
    node _firstIndex_T = add(groupOffset, butterflyCounter) @[FFT.scala 106:38]
    node firstIndex = tail(_firstIndex_T, 1) @[FFT.scala 106:38]
    node _secondIndex_T = add(firstIndex, stageSize) @[FFT.scala 107:38]
    node secondIndex = tail(_secondIndex_T, 1) @[FFT.scala 107:38]
    node _twiddleIndex_T = add(stageCounter, UInt<1>("h1")) @[FFT.scala 109:78]
    node _twiddleIndex_T_1 = tail(_twiddleIndex_T, 1) @[FFT.scala 109:78]
    node _twiddleIndex_T_2 = dshr(UInt<4>("h8"), _twiddleIndex_T_1) @[FFT.scala 109:61]
    node twiddleIndex = mul(butterflyCounter, _twiddleIndex_T_2) @[FFT.scala 109:46]
    node _T_3 = bits(firstIndex, 2, 0)
    node _T_4 = bits(secondIndex, 2, 0)
    node _T_5 = bits(twiddleIndex, 1, 0)
    node _GEN_20 = validif(eq(UInt<1>("h0"), _T_4), dataRegs_0_real) @[FFT.scala 48:{25,25}]
    node _GEN_21 = mux(eq(UInt<1>("h1"), _T_4), dataRegs_1_real, _GEN_20) @[FFT.scala 48:{25,25}]
    node _GEN_22 = mux(eq(UInt<2>("h2"), _T_4), dataRegs_2_real, _GEN_21) @[FFT.scala 48:{25,25}]
    node _GEN_23 = mux(eq(UInt<2>("h3"), _T_4), dataRegs_3_real, _GEN_22) @[FFT.scala 48:{25,25}]
    node _GEN_24 = mux(eq(UInt<3>("h4"), _T_4), dataRegs_4_real, _GEN_23) @[FFT.scala 48:{25,25}]
    node _GEN_25 = mux(eq(UInt<3>("h5"), _T_4), dataRegs_5_real, _GEN_24) @[FFT.scala 48:{25,25}]
    node _GEN_26 = mux(eq(UInt<3>("h6"), _T_4), dataRegs_6_real, _GEN_25) @[FFT.scala 48:{25,25}]
    node _GEN_27 = mux(eq(UInt<3>("h7"), _T_4), dataRegs_7_real, _GEN_26) @[FFT.scala 48:{25,25}]
    node twiddleFactors_twiddle_real = asSInt(UInt<32>("h400")) @[FFT.scala 37:23 38:18]
    node twiddleFactors_0_real = twiddleFactors_twiddle_real @[FFT.scala 36:{31,31}]
    node _GEN_28 = validif(eq(UInt<1>("h0"), _T_5), twiddleFactors_0_real) @[FFT.scala 48:{25,25}]
    node twiddleFactors_twiddle_1_real = asSInt(UInt<32>("h2d4")) @[FFT.scala 37:23 38:18]
    node twiddleFactors_1_real = twiddleFactors_twiddle_1_real @[FFT.scala 36:{31,31}]
    node _GEN_29 = mux(eq(UInt<1>("h1"), _T_5), twiddleFactors_1_real, _GEN_28) @[FFT.scala 48:{25,25}]
    node twiddleFactors_twiddle_2_real = asSInt(UInt<32>("h0")) @[FFT.scala 37:23 38:18]
    node twiddleFactors_2_real = twiddleFactors_twiddle_2_real @[FFT.scala 36:{31,31}]
    node _GEN_30 = mux(eq(UInt<2>("h2"), _T_5), twiddleFactors_2_real, _GEN_29) @[FFT.scala 48:{25,25}]
    node twiddleFactors_twiddle_3_real = asSInt(UInt<32>("hfffffd2c")) @[FFT.scala 37:23 38:18]
    node twiddleFactors_3_real = twiddleFactors_twiddle_3_real @[FFT.scala 36:{31,31}]
    node _GEN_31 = mux(eq(UInt<2>("h3"), _T_5), twiddleFactors_3_real, _GEN_30) @[FFT.scala 48:{25,25}]
    node _dataRegs_T_4_real = _GEN_27 @[FFT.scala 48:25]
    node _twiddleFactors_T_5_real = _GEN_31 @[FFT.scala 48:25]
    node _product_tempReal_T = mul(_dataRegs_T_4_real, _twiddleFactors_T_5_real) @[FFT.scala 48:25]
    node _GEN_32 = validif(eq(UInt<1>("h0"), _T_4), dataRegs_0_imag) @[FFT.scala 48:{43,43}]
    node _GEN_33 = mux(eq(UInt<1>("h1"), _T_4), dataRegs_1_imag, _GEN_32) @[FFT.scala 48:{43,43}]
    node _GEN_34 = mux(eq(UInt<2>("h2"), _T_4), dataRegs_2_imag, _GEN_33) @[FFT.scala 48:{43,43}]
    node _GEN_35 = mux(eq(UInt<2>("h3"), _T_4), dataRegs_3_imag, _GEN_34) @[FFT.scala 48:{43,43}]
    node _GEN_36 = mux(eq(UInt<3>("h4"), _T_4), dataRegs_4_imag, _GEN_35) @[FFT.scala 48:{43,43}]
    node _GEN_37 = mux(eq(UInt<3>("h5"), _T_4), dataRegs_5_imag, _GEN_36) @[FFT.scala 48:{43,43}]
    node _GEN_38 = mux(eq(UInt<3>("h6"), _T_4), dataRegs_6_imag, _GEN_37) @[FFT.scala 48:{43,43}]
    node _GEN_39 = mux(eq(UInt<3>("h7"), _T_4), dataRegs_7_imag, _GEN_38) @[FFT.scala 48:{43,43}]
    node twiddleFactors_twiddle_imag = asSInt(UInt<32>("h0")) @[FFT.scala 37:23 39:18]
    node twiddleFactors_0_imag = twiddleFactors_twiddle_imag @[FFT.scala 36:{31,31}]
    node _GEN_40 = validif(eq(UInt<1>("h0"), _T_5), twiddleFactors_0_imag) @[FFT.scala 48:{43,43}]
    node twiddleFactors_twiddle_1_imag = asSInt(UInt<32>("hfffffd2c")) @[FFT.scala 37:23 39:18]
    node twiddleFactors_1_imag = twiddleFactors_twiddle_1_imag @[FFT.scala 36:{31,31}]
    node _GEN_41 = mux(eq(UInt<1>("h1"), _T_5), twiddleFactors_1_imag, _GEN_40) @[FFT.scala 48:{43,43}]
    node twiddleFactors_twiddle_2_imag = asSInt(UInt<32>("hfffffc00")) @[FFT.scala 37:23 39:18]
    node twiddleFactors_2_imag = twiddleFactors_twiddle_2_imag @[FFT.scala 36:{31,31}]
    node _GEN_42 = mux(eq(UInt<2>("h2"), _T_5), twiddleFactors_2_imag, _GEN_41) @[FFT.scala 48:{43,43}]
    node twiddleFactors_twiddle_3_imag = asSInt(UInt<32>("hfffffd2c")) @[FFT.scala 37:23 39:18]
    node twiddleFactors_3_imag = twiddleFactors_twiddle_3_imag @[FFT.scala 36:{31,31}]
    node _GEN_43 = mux(eq(UInt<2>("h3"), _T_5), twiddleFactors_3_imag, _GEN_42) @[FFT.scala 48:{43,43}]
    node _dataRegs_T_4_imag = _GEN_39 @[FFT.scala 48:43]
    node _twiddleFactors_T_5_imag = _GEN_43 @[FFT.scala 48:43]
    node _product_tempReal_T_1 = mul(_dataRegs_T_4_imag, _twiddleFactors_T_5_imag) @[FFT.scala 48:43]
    node _product_tempReal_T_2 = sub(_product_tempReal_T, _product_tempReal_T_1) @[FFT.scala 48:34]
    node _product_tempReal_T_3 = tail(_product_tempReal_T_2, 1) @[FFT.scala 48:34]
    node _product_tempReal_T_4 = asSInt(_product_tempReal_T_3) @[FFT.scala 48:34]
    node _dataRegs_T_4_real_0 = _GEN_27 @[FFT.scala 49:25]
    node _twiddleFactors_T_5_imag_0 = _GEN_43 @[FFT.scala 49:25]
    node _product_tempImag_T = mul(_dataRegs_T_4_real_0, _twiddleFactors_T_5_imag_0) @[FFT.scala 49:25]
    node _dataRegs_T_4_imag_0 = _GEN_39 @[FFT.scala 49:43]
    node _twiddleFactors_T_5_real_0 = _GEN_31 @[FFT.scala 49:43]
    node _product_tempImag_T_1 = mul(_dataRegs_T_4_imag_0, _twiddleFactors_T_5_real_0) @[FFT.scala 49:43]
    node _product_tempImag_T_2 = add(_product_tempImag_T, _product_tempImag_T_1) @[FFT.scala 49:34]
    node _product_tempImag_T_3 = tail(_product_tempImag_T_2, 1) @[FFT.scala 49:34]
    node _product_tempImag_T_4 = asSInt(_product_tempImag_T_3) @[FFT.scala 49:34]
    node product_roundingFactor = shl(asSInt(UInt<2>("h1")), 9) @[FFT.scala 52:31]
    node product_tempReal = _product_tempReal_T_4 @[FFT.scala 45:24 48:14]
    node _product_result_real_T = add(product_tempReal, product_roundingFactor) @[FFT.scala 53:31]
    node _product_result_real_T_1 = shr(_product_result_real_T, 10) @[FFT.scala 53:50]
    node product_tempImag = _product_tempImag_T_4 @[FFT.scala 46:24 49:14]
    node _product_result_imag_T = add(product_tempImag, product_roundingFactor) @[FFT.scala 54:31]
    node _product_result_imag_T_1 = shr(_product_result_imag_T, 10) @[FFT.scala 54:50]
    node _GEN_44 = validif(eq(UInt<1>("h0"), _T_3), dataRegs_0_real) @[FFT.scala 65:{28,28}]
    node _GEN_45 = mux(eq(UInt<1>("h1"), _T_3), dataRegs_1_real, _GEN_44) @[FFT.scala 65:{28,28}]
    node _GEN_46 = mux(eq(UInt<2>("h2"), _T_3), dataRegs_2_real, _GEN_45) @[FFT.scala 65:{28,28}]
    node _GEN_47 = mux(eq(UInt<2>("h3"), _T_3), dataRegs_3_real, _GEN_46) @[FFT.scala 65:{28,28}]
    node _GEN_48 = mux(eq(UInt<3>("h4"), _T_3), dataRegs_4_real, _GEN_47) @[FFT.scala 65:{28,28}]
    node _GEN_49 = mux(eq(UInt<3>("h5"), _T_3), dataRegs_5_real, _GEN_48) @[FFT.scala 65:{28,28}]
    node _GEN_50 = mux(eq(UInt<3>("h6"), _T_3), dataRegs_6_real, _GEN_49) @[FFT.scala 65:{28,28}]
    node _GEN_51 = mux(eq(UInt<3>("h7"), _T_3), dataRegs_7_real, _GEN_50) @[FFT.scala 65:{28,28}]
    node _dataRegs_T_3_real = _GEN_51 @[FFT.scala 65:28]
    node product_real = asSInt(bits(_product_result_real_T_1, 31, 0)) @[FFT.scala 44:22 53:17]
    node _out0_real_T = add(_dataRegs_T_3_real, product_real) @[FFT.scala 65:28]
    node _GEN_52 = validif(eq(UInt<1>("h0"), _T_3), dataRegs_0_imag) @[FFT.scala 66:{28,28}]
    node _GEN_53 = mux(eq(UInt<1>("h1"), _T_3), dataRegs_1_imag, _GEN_52) @[FFT.scala 66:{28,28}]
    node _GEN_54 = mux(eq(UInt<2>("h2"), _T_3), dataRegs_2_imag, _GEN_53) @[FFT.scala 66:{28,28}]
    node _GEN_55 = mux(eq(UInt<2>("h3"), _T_3), dataRegs_3_imag, _GEN_54) @[FFT.scala 66:{28,28}]
    node _GEN_56 = mux(eq(UInt<3>("h4"), _T_3), dataRegs_4_imag, _GEN_55) @[FFT.scala 66:{28,28}]
    node _GEN_57 = mux(eq(UInt<3>("h5"), _T_3), dataRegs_5_imag, _GEN_56) @[FFT.scala 66:{28,28}]
    node _GEN_58 = mux(eq(UInt<3>("h6"), _T_3), dataRegs_6_imag, _GEN_57) @[FFT.scala 66:{28,28}]
    node _GEN_59 = mux(eq(UInt<3>("h7"), _T_3), dataRegs_7_imag, _GEN_58) @[FFT.scala 66:{28,28}]
    node _dataRegs_T_3_imag = _GEN_59 @[FFT.scala 66:28]
    node product_imag = asSInt(bits(_product_result_imag_T_1, 31, 0)) @[FFT.scala 44:22 54:17]
    node _out0_imag_T = add(_dataRegs_T_3_imag, product_imag) @[FFT.scala 66:28]
    node _dataRegs_T_3_real_0 = _GEN_51 @[FFT.scala 67:28]
    node _out1_real_T = sub(_dataRegs_T_3_real_0, product_real) @[FFT.scala 67:28]
    node _dataRegs_T_3_imag_0 = _GEN_59 @[FFT.scala 68:28]
    node _out1_imag_T = sub(_dataRegs_T_3_imag_0, product_imag) @[FFT.scala 68:28]
    node _T_6 = bits(firstIndex, 2, 0)
    node out0_real = asSInt(bits(_out0_real_T, 31, 0)) @[FFT.scala 61:20 65:15]
    node _dataRegs_T_6_real = out0_real @[FFT.scala 116:{30,30}]
    node _GEN_60 = mux(eq(UInt<1>("h0"), _T_6), _dataRegs_T_6_real, dataRegs_0_real) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_61 = mux(eq(UInt<1>("h1"), _T_6), _dataRegs_T_6_real, dataRegs_1_real) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_62 = mux(eq(UInt<2>("h2"), _T_6), _dataRegs_T_6_real, dataRegs_2_real) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_63 = mux(eq(UInt<2>("h3"), _T_6), _dataRegs_T_6_real, dataRegs_3_real) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_64 = mux(eq(UInt<3>("h4"), _T_6), _dataRegs_T_6_real, dataRegs_4_real) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_65 = mux(eq(UInt<3>("h5"), _T_6), _dataRegs_T_6_real, dataRegs_5_real) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_66 = mux(eq(UInt<3>("h6"), _T_6), _dataRegs_T_6_real, dataRegs_6_real) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_67 = mux(eq(UInt<3>("h7"), _T_6), _dataRegs_T_6_real, dataRegs_7_real) @[FFT.scala 116:{30,30} 73:25]
    node out0_imag = asSInt(bits(_out0_imag_T, 31, 0)) @[FFT.scala 61:20 66:15]
    node _dataRegs_T_6_imag = out0_imag @[FFT.scala 116:{30,30}]
    node _GEN_68 = mux(eq(UInt<1>("h0"), _T_6), _dataRegs_T_6_imag, dataRegs_0_imag) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_69 = mux(eq(UInt<1>("h1"), _T_6), _dataRegs_T_6_imag, dataRegs_1_imag) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_70 = mux(eq(UInt<2>("h2"), _T_6), _dataRegs_T_6_imag, dataRegs_2_imag) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_71 = mux(eq(UInt<2>("h3"), _T_6), _dataRegs_T_6_imag, dataRegs_3_imag) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_72 = mux(eq(UInt<3>("h4"), _T_6), _dataRegs_T_6_imag, dataRegs_4_imag) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_73 = mux(eq(UInt<3>("h5"), _T_6), _dataRegs_T_6_imag, dataRegs_5_imag) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_74 = mux(eq(UInt<3>("h6"), _T_6), _dataRegs_T_6_imag, dataRegs_6_imag) @[FFT.scala 116:{30,30} 73:25]
    node _GEN_75 = mux(eq(UInt<3>("h7"), _T_6), _dataRegs_T_6_imag, dataRegs_7_imag) @[FFT.scala 116:{30,30} 73:25]
    node _T_7 = bits(secondIndex, 2, 0)
    node out1_real = asSInt(bits(_out1_real_T, 31, 0)) @[FFT.scala 62:20 67:15]
    node _dataRegs_T_7_real = out1_real @[FFT.scala 117:{31,31}]
    node _GEN_76 = mux(eq(UInt<1>("h0"), _T_7), _dataRegs_T_7_real, _GEN_60) @[FFT.scala 117:{31,31}]
    node _GEN_77 = mux(eq(UInt<1>("h1"), _T_7), _dataRegs_T_7_real, _GEN_61) @[FFT.scala 117:{31,31}]
    node _GEN_78 = mux(eq(UInt<2>("h2"), _T_7), _dataRegs_T_7_real, _GEN_62) @[FFT.scala 117:{31,31}]
    node _GEN_79 = mux(eq(UInt<2>("h3"), _T_7), _dataRegs_T_7_real, _GEN_63) @[FFT.scala 117:{31,31}]
    node _GEN_80 = mux(eq(UInt<3>("h4"), _T_7), _dataRegs_T_7_real, _GEN_64) @[FFT.scala 117:{31,31}]
    node _GEN_81 = mux(eq(UInt<3>("h5"), _T_7), _dataRegs_T_7_real, _GEN_65) @[FFT.scala 117:{31,31}]
    node _GEN_82 = mux(eq(UInt<3>("h6"), _T_7), _dataRegs_T_7_real, _GEN_66) @[FFT.scala 117:{31,31}]
    node _GEN_83 = mux(eq(UInt<3>("h7"), _T_7), _dataRegs_T_7_real, _GEN_67) @[FFT.scala 117:{31,31}]
    node out1_imag = asSInt(bits(_out1_imag_T, 31, 0)) @[FFT.scala 62:20 68:15]
    node _dataRegs_T_7_imag = out1_imag @[FFT.scala 117:{31,31}]
    node _GEN_84 = mux(eq(UInt<1>("h0"), _T_7), _dataRegs_T_7_imag, _GEN_68) @[FFT.scala 117:{31,31}]
    node _GEN_85 = mux(eq(UInt<1>("h1"), _T_7), _dataRegs_T_7_imag, _GEN_69) @[FFT.scala 117:{31,31}]
    node _GEN_86 = mux(eq(UInt<2>("h2"), _T_7), _dataRegs_T_7_imag, _GEN_70) @[FFT.scala 117:{31,31}]
    node _GEN_87 = mux(eq(UInt<2>("h3"), _T_7), _dataRegs_T_7_imag, _GEN_71) @[FFT.scala 117:{31,31}]
    node _GEN_88 = mux(eq(UInt<3>("h4"), _T_7), _dataRegs_T_7_imag, _GEN_72) @[FFT.scala 117:{31,31}]
    node _GEN_89 = mux(eq(UInt<3>("h5"), _T_7), _dataRegs_T_7_imag, _GEN_73) @[FFT.scala 117:{31,31}]
    node _GEN_90 = mux(eq(UInt<3>("h6"), _T_7), _dataRegs_T_7_imag, _GEN_74) @[FFT.scala 117:{31,31}]
    node _GEN_91 = mux(eq(UInt<3>("h7"), _T_7), _dataRegs_T_7_imag, _GEN_75) @[FFT.scala 117:{31,31}]
    node _T_8 = sub(stageSize, UInt<1>("h1")) @[FFT.scala 119:46]
    node _T_9 = tail(_T_8, 1) @[FFT.scala 119:46]
    node _T_10 = eq(butterflyCounter, _T_9) @[FFT.scala 119:31]
    node _T_11 = div(UInt<4>("h8"), groupSize) @[FFT.scala 121:47]
    node _T_12 = sub(_T_11, UInt<1>("h1")) @[FFT.scala 121:60]
    node _T_13 = tail(_T_12, 1) @[FFT.scala 121:60]
    node _T_14 = eq(groupCounter, _T_13) @[FFT.scala 121:29]
    node _stageCounter_T = add(stageCounter, UInt<1>("h1")) @[FFT.scala 123:42]
    node _stageCounter_T_1 = tail(_stageCounter_T, 1) @[FFT.scala 123:42]
    node _groupCounter_T = add(groupCounter, UInt<1>("h1")) @[FFT.scala 125:42]
    node _groupCounter_T_1 = tail(_groupCounter_T, 1) @[FFT.scala 125:42]
    node _GEN_92 = mux(_T_14, UInt<1>("h0"), _groupCounter_T_1) @[FFT.scala 121:68 122:26 125:26]
    node _GEN_93 = mux(_T_14, _stageCounter_T_1, stageCounter) @[FFT.scala 121:68 123:26 74:29]
    node _butterflyCounter_T = add(butterflyCounter, UInt<1>("h1")) @[FFT.scala 128:48]
    node _butterflyCounter_T_1 = tail(_butterflyCounter_T, 1) @[FFT.scala 128:48]
    node _GEN_94 = mux(_T_10, UInt<1>("h0"), _butterflyCounter_T_1) @[FFT.scala 119:54 120:28 128:28]
    node _GEN_95 = mux(_T_10, _GEN_92, groupCounter) @[FFT.scala 119:54 75:29]
    node _GEN_96 = mux(_T_10, _GEN_93, stageCounter) @[FFT.scala 119:54 74:29]
    node _GEN_97 = mux(_T_2, _GEN_76, dataRegs_0_real) @[FFT.scala 100:40 73:25]
    node _GEN_98 = mux(_T_2, _GEN_77, dataRegs_1_real) @[FFT.scala 100:40 73:25]
    node _GEN_99 = mux(_T_2, _GEN_78, dataRegs_2_real) @[FFT.scala 100:40 73:25]
    node _GEN_100 = mux(_T_2, _GEN_79, dataRegs_3_real) @[FFT.scala 100:40 73:25]
    node _GEN_101 = mux(_T_2, _GEN_80, dataRegs_4_real) @[FFT.scala 100:40 73:25]
    node _GEN_102 = mux(_T_2, _GEN_81, dataRegs_5_real) @[FFT.scala 100:40 73:25]
    node _GEN_103 = mux(_T_2, _GEN_82, dataRegs_6_real) @[FFT.scala 100:40 73:25]
    node _GEN_104 = mux(_T_2, _GEN_83, dataRegs_7_real) @[FFT.scala 100:40 73:25]
    node _GEN_105 = mux(_T_2, _GEN_84, dataRegs_0_imag) @[FFT.scala 100:40 73:25]
    node _GEN_106 = mux(_T_2, _GEN_85, dataRegs_1_imag) @[FFT.scala 100:40 73:25]
    node _GEN_107 = mux(_T_2, _GEN_86, dataRegs_2_imag) @[FFT.scala 100:40 73:25]
    node _GEN_108 = mux(_T_2, _GEN_87, dataRegs_3_imag) @[FFT.scala 100:40 73:25]
    node _GEN_109 = mux(_T_2, _GEN_88, dataRegs_4_imag) @[FFT.scala 100:40 73:25]
    node _GEN_110 = mux(_T_2, _GEN_89, dataRegs_5_imag) @[FFT.scala 100:40 73:25]
    node _GEN_111 = mux(_T_2, _GEN_90, dataRegs_6_imag) @[FFT.scala 100:40 73:25]
    node _GEN_112 = mux(_T_2, _GEN_91, dataRegs_7_imag) @[FFT.scala 100:40 73:25]
    node _GEN_113 = mux(_T_2, _GEN_94, butterflyCounter) @[FFT.scala 100:40 76:33]
    node _GEN_114 = mux(_T_2, _GEN_95, groupCounter) @[FFT.scala 100:40 75:29]
    node _GEN_115 = mux(_T_2, _GEN_96, stageCounter) @[FFT.scala 100:40 74:29]
    node _GEN_116 = mux(_T_2, state, UInt<2>("h2")) @[FFT.scala 100:40 131:15 79:22]
    node _T_15 = eq(UInt<2>("h2"), state) @[FFT.scala 84:17]
    node _T_16 = eq(io_start, UInt<1>("h0")) @[FFT.scala 136:12]
    node _GEN_117 = mux(_T_16, UInt<2>("h0"), state) @[FFT.scala 136:23 137:15 79:22]
    node _GEN_118 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_0_real) @[FFT.scala 136:23 139:20 73:25]
    node _GEN_119 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_0_imag) @[FFT.scala 136:23 140:20 73:25]
    node _GEN_120 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_1_real) @[FFT.scala 136:23 139:20 73:25]
    node _GEN_121 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_1_imag) @[FFT.scala 136:23 140:20 73:25]
    node _GEN_122 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_2_real) @[FFT.scala 136:23 139:20 73:25]
    node _GEN_123 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_2_imag) @[FFT.scala 136:23 140:20 73:25]
    node _GEN_124 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_3_real) @[FFT.scala 136:23 139:20 73:25]
    node _GEN_125 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_3_imag) @[FFT.scala 136:23 140:20 73:25]
    node _GEN_126 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_4_real) @[FFT.scala 136:23 139:20 73:25]
    node _GEN_127 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_4_imag) @[FFT.scala 136:23 140:20 73:25]
    node _GEN_128 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_5_real) @[FFT.scala 136:23 139:20 73:25]
    node _GEN_129 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_5_imag) @[FFT.scala 136:23 140:20 73:25]
    node _GEN_130 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_6_real) @[FFT.scala 136:23 139:20 73:25]
    node _GEN_131 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_6_imag) @[FFT.scala 136:23 140:20 73:25]
    node _GEN_132 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_7_real) @[FFT.scala 136:23 139:20 73:25]
    node _GEN_133 = mux(_T_16, asSInt(UInt<1>("h0")), dataRegs_7_imag) @[FFT.scala 136:23 140:20 73:25]
    node _GEN_134 = mux(_T_15, _GEN_117, state) @[FFT.scala 84:17 79:22]
    node _GEN_135 = mux(_T_15, _GEN_118, dataRegs_0_real) @[FFT.scala 84:17 73:25]
    node _GEN_136 = mux(_T_15, _GEN_119, dataRegs_0_imag) @[FFT.scala 84:17 73:25]
    node _GEN_137 = mux(_T_15, _GEN_120, dataRegs_1_real) @[FFT.scala 84:17 73:25]
    node _GEN_138 = mux(_T_15, _GEN_121, dataRegs_1_imag) @[FFT.scala 84:17 73:25]
    node _GEN_139 = mux(_T_15, _GEN_122, dataRegs_2_real) @[FFT.scala 84:17 73:25]
    node _GEN_140 = mux(_T_15, _GEN_123, dataRegs_2_imag) @[FFT.scala 84:17 73:25]
    node _GEN_141 = mux(_T_15, _GEN_124, dataRegs_3_real) @[FFT.scala 84:17 73:25]
    node _GEN_142 = mux(_T_15, _GEN_125, dataRegs_3_imag) @[FFT.scala 84:17 73:25]
    node _GEN_143 = mux(_T_15, _GEN_126, dataRegs_4_real) @[FFT.scala 84:17 73:25]
    node _GEN_144 = mux(_T_15, _GEN_127, dataRegs_4_imag) @[FFT.scala 84:17 73:25]
    node _GEN_145 = mux(_T_15, _GEN_128, dataRegs_5_real) @[FFT.scala 84:17 73:25]
    node _GEN_146 = mux(_T_15, _GEN_129, dataRegs_5_imag) @[FFT.scala 84:17 73:25]
    node _GEN_147 = mux(_T_15, _GEN_130, dataRegs_6_real) @[FFT.scala 84:17 73:25]
    node _GEN_148 = mux(_T_15, _GEN_131, dataRegs_6_imag) @[FFT.scala 84:17 73:25]
    node _GEN_149 = mux(_T_15, _GEN_132, dataRegs_7_real) @[FFT.scala 84:17 73:25]
    node _GEN_150 = mux(_T_15, _GEN_133, dataRegs_7_imag) @[FFT.scala 84:17 73:25]
    node _GEN_151 = mux(_T_1, _GEN_97, _GEN_135) @[FFT.scala 84:17]
    node _GEN_152 = mux(_T_1, _GEN_98, _GEN_137) @[FFT.scala 84:17]
    node _GEN_153 = mux(_T_1, _GEN_99, _GEN_139) @[FFT.scala 84:17]
    node _GEN_154 = mux(_T_1, _GEN_100, _GEN_141) @[FFT.scala 84:17]
    node _GEN_155 = mux(_T_1, _GEN_101, _GEN_143) @[FFT.scala 84:17]
    node _GEN_156 = mux(_T_1, _GEN_102, _GEN_145) @[FFT.scala 84:17]
    node _GEN_157 = mux(_T_1, _GEN_103, _GEN_147) @[FFT.scala 84:17]
    node _GEN_158 = mux(_T_1, _GEN_104, _GEN_149) @[FFT.scala 84:17]
    node _GEN_159 = mux(_T_1, _GEN_105, _GEN_136) @[FFT.scala 84:17]
    node _GEN_160 = mux(_T_1, _GEN_106, _GEN_138) @[FFT.scala 84:17]
    node _GEN_161 = mux(_T_1, _GEN_107, _GEN_140) @[FFT.scala 84:17]
    node _GEN_162 = mux(_T_1, _GEN_108, _GEN_142) @[FFT.scala 84:17]
    node _GEN_163 = mux(_T_1, _GEN_109, _GEN_144) @[FFT.scala 84:17]
    node _GEN_164 = mux(_T_1, _GEN_110, _GEN_146) @[FFT.scala 84:17]
    node _GEN_165 = mux(_T_1, _GEN_111, _GEN_148) @[FFT.scala 84:17]
    node _GEN_166 = mux(_T_1, _GEN_112, _GEN_150) @[FFT.scala 84:17]
    node _GEN_167 = mux(_T_1, _GEN_113, butterflyCounter) @[FFT.scala 84:17 76:33]
    node _GEN_168 = mux(_T_1, _GEN_114, groupCounter) @[FFT.scala 84:17 75:29]
    node _GEN_169 = mux(_T_1, _GEN_115, stageCounter) @[FFT.scala 84:17 74:29]
    node _GEN_170 = mux(_T_1, _GEN_116, _GEN_134) @[FFT.scala 84:17]
    node _GEN_171 = mux(_T, _GEN_0, _GEN_151) @[FFT.scala 84:17]
    node _GEN_172 = mux(_T, _GEN_1, _GEN_159) @[FFT.scala 84:17]
    node _GEN_173 = mux(_T, _GEN_2, _GEN_152) @[FFT.scala 84:17]
    node _GEN_174 = mux(_T, _GEN_3, _GEN_160) @[FFT.scala 84:17]
    node _GEN_175 = mux(_T, _GEN_4, _GEN_153) @[FFT.scala 84:17]
    node _GEN_176 = mux(_T, _GEN_5, _GEN_161) @[FFT.scala 84:17]
    node _GEN_177 = mux(_T, _GEN_6, _GEN_154) @[FFT.scala 84:17]
    node _GEN_178 = mux(_T, _GEN_7, _GEN_162) @[FFT.scala 84:17]
    node _GEN_179 = mux(_T, _GEN_8, _GEN_155) @[FFT.scala 84:17]
    node _GEN_180 = mux(_T, _GEN_9, _GEN_163) @[FFT.scala 84:17]
    node _GEN_181 = mux(_T, _GEN_10, _GEN_156) @[FFT.scala 84:17]
    node _GEN_182 = mux(_T, _GEN_11, _GEN_164) @[FFT.scala 84:17]
    node _GEN_183 = mux(_T, _GEN_12, _GEN_157) @[FFT.scala 84:17]
    node _GEN_184 = mux(_T, _GEN_13, _GEN_165) @[FFT.scala 84:17]
    node _GEN_185 = mux(_T, _GEN_14, _GEN_158) @[FFT.scala 84:17]
    node _GEN_186 = mux(_T, _GEN_15, _GEN_166) @[FFT.scala 84:17]
    node _GEN_187 = mux(_T, _GEN_16, _GEN_169) @[FFT.scala 84:17]
    node _GEN_188 = mux(_T, _GEN_17, _GEN_168) @[FFT.scala 84:17]
    node _GEN_189 = mux(_T, _GEN_18, _GEN_167) @[FFT.scala 84:17]
    node _GEN_190 = mux(_T, _GEN_19, _GEN_170) @[FFT.scala 84:17]
    node _dataRegs_WIRE_real = _dataRegs_T_4 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_imag = _dataRegs_T_2 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_2_real = _dataRegs_T_9 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_2_imag = _dataRegs_T_7 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_4_real = _dataRegs_T_14 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_4_imag = _dataRegs_T_12 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_6_real = _dataRegs_T_19 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_6_imag = _dataRegs_T_17 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_8_real = _dataRegs_T_24 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_8_imag = _dataRegs_T_22 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_10_real = _dataRegs_T_29 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_10_imag = _dataRegs_T_27 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_12_real = _dataRegs_T_34 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_12_imag = _dataRegs_T_32 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_14_real = _dataRegs_T_39 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_14_imag = _dataRegs_T_37 @[FFT.scala 73:{72,72}]
    node _dataRegs_WIRE_16_0_real = _dataRegs_WIRE_real @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_0_imag = _dataRegs_WIRE_imag @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_1_real = _dataRegs_WIRE_2_real @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_1_imag = _dataRegs_WIRE_2_imag @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_2_real = _dataRegs_WIRE_4_real @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_2_imag = _dataRegs_WIRE_4_imag @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_3_real = _dataRegs_WIRE_6_real @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_3_imag = _dataRegs_WIRE_6_imag @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_4_real = _dataRegs_WIRE_8_real @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_4_imag = _dataRegs_WIRE_8_imag @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_5_real = _dataRegs_WIRE_10_real @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_5_imag = _dataRegs_WIRE_10_imag @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_6_real = _dataRegs_WIRE_12_real @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_6_imag = _dataRegs_WIRE_12_imag @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_7_real = _dataRegs_WIRE_14_real @[FFT.scala 73:{33,33}]
    node _dataRegs_WIRE_16_7_imag = _dataRegs_WIRE_14_imag @[FFT.scala 73:{33,33}]
    io_out_0_real <= dataRegs_0_real @[FFT.scala 82:10]
    io_out_0_imag <= dataRegs_0_imag @[FFT.scala 82:10]
    io_out_1_real <= dataRegs_1_real @[FFT.scala 82:10]
    io_out_1_imag <= dataRegs_1_imag @[FFT.scala 82:10]
    io_out_2_real <= dataRegs_2_real @[FFT.scala 82:10]
    io_out_2_imag <= dataRegs_2_imag @[FFT.scala 82:10]
    io_out_3_real <= dataRegs_3_real @[FFT.scala 82:10]
    io_out_3_imag <= dataRegs_3_imag @[FFT.scala 82:10]
    io_out_4_real <= dataRegs_4_real @[FFT.scala 82:10]
    io_out_4_imag <= dataRegs_4_imag @[FFT.scala 82:10]
    io_out_5_real <= dataRegs_5_real @[FFT.scala 82:10]
    io_out_5_imag <= dataRegs_5_imag @[FFT.scala 82:10]
    io_out_6_real <= dataRegs_6_real @[FFT.scala 82:10]
    io_out_6_imag <= dataRegs_6_imag @[FFT.scala 82:10]
    io_out_7_real <= dataRegs_7_real @[FFT.scala 82:10]
    io_out_7_imag <= dataRegs_7_imag @[FFT.scala 82:10]
    io_done <= _io_done_T @[FFT.scala 81:11]
    dataRegs_0_real <= mux(reset, _dataRegs_WIRE_16_0_real, _GEN_171) @[FFT.scala 73:{25,25}]
    dataRegs_0_imag <= mux(reset, _dataRegs_WIRE_16_0_imag, _GEN_172) @[FFT.scala 73:{25,25}]
    dataRegs_1_real <= mux(reset, _dataRegs_WIRE_16_1_real, _GEN_173) @[FFT.scala 73:{25,25}]
    dataRegs_1_imag <= mux(reset, _dataRegs_WIRE_16_1_imag, _GEN_174) @[FFT.scala 73:{25,25}]
    dataRegs_2_real <= mux(reset, _dataRegs_WIRE_16_2_real, _GEN_175) @[FFT.scala 73:{25,25}]
    dataRegs_2_imag <= mux(reset, _dataRegs_WIRE_16_2_imag, _GEN_176) @[FFT.scala 73:{25,25}]
    dataRegs_3_real <= mux(reset, _dataRegs_WIRE_16_3_real, _GEN_177) @[FFT.scala 73:{25,25}]
    dataRegs_3_imag <= mux(reset, _dataRegs_WIRE_16_3_imag, _GEN_178) @[FFT.scala 73:{25,25}]
    dataRegs_4_real <= mux(reset, _dataRegs_WIRE_16_4_real, _GEN_179) @[FFT.scala 73:{25,25}]
    dataRegs_4_imag <= mux(reset, _dataRegs_WIRE_16_4_imag, _GEN_180) @[FFT.scala 73:{25,25}]
    dataRegs_5_real <= mux(reset, _dataRegs_WIRE_16_5_real, _GEN_181) @[FFT.scala 73:{25,25}]
    dataRegs_5_imag <= mux(reset, _dataRegs_WIRE_16_5_imag, _GEN_182) @[FFT.scala 73:{25,25}]
    dataRegs_6_real <= mux(reset, _dataRegs_WIRE_16_6_real, _GEN_183) @[FFT.scala 73:{25,25}]
    dataRegs_6_imag <= mux(reset, _dataRegs_WIRE_16_6_imag, _GEN_184) @[FFT.scala 73:{25,25}]
    dataRegs_7_real <= mux(reset, _dataRegs_WIRE_16_7_real, _GEN_185) @[FFT.scala 73:{25,25}]
    dataRegs_7_imag <= mux(reset, _dataRegs_WIRE_16_7_imag, _GEN_186) @[FFT.scala 73:{25,25}]
    stageCounter <= mux(reset, UInt<2>("h0"), _GEN_187) @[FFT.scala 74:{29,29}]
    groupCounter <= mux(reset, UInt<3>("h0"), _GEN_188) @[FFT.scala 75:{29,29}]
    butterflyCounter <= mux(reset, UInt<2>("h0"), _GEN_189) @[FFT.scala 76:{33,33}]
    state <= mux(reset, UInt<2>("h0"), _GEN_190) @[FFT.scala 79:{22,22}]
