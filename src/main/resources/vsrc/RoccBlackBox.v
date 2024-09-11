// See LICENSE.SiFive for license details.

module RoccBlackBox
  #( parameter xLen = 64,
     PRV_SZ = 2,
     coreMaxAddrBits = 40,
     dcacheReqTagBits = 9,
     M_SZ = 5,
     mem_req_bits_size_width = 2,
     coreDataBits = 64,
     coreDataBytes = 8,
     paddrBits = 32,
     vaddrBitsExtended = 40,
     FPConstants_RM_SZ = 3,
     fLen = 64,
     FPConstants_FLAGS_SZ  = 5)
  ( input clock,
    input reset,
    output rocc_cmd_ready,
    input rocc_cmd_valid,
    input [6:0] rocc_cmd_bits_inst_funct,
    input [4:0] rocc_cmd_bits_inst_rs2,
    input [4:0] rocc_cmd_bits_inst_rs1,
    input rocc_cmd_bits_inst_xd,
    input rocc_cmd_bits_inst_xs1,
    input rocc_cmd_bits_inst_xs2,
    input [4:0] rocc_cmd_bits_inst_rd,
    input [6:0] rocc_cmd_bits_inst_opcode,
    input [xLen-1:0] rocc_cmd_bits_rs1,
    input [xLen-1:0] rocc_cmd_bits_rs2,
    input rocc_cmd_bits_status_debug,
    input rocc_cmd_bits_status_cease,
    input rocc_cmd_bits_status_wfi,
    input [31:0] rocc_cmd_bits_status_isa,
    input [PRV_SZ-1:0] rocc_cmd_bits_status_dprv,
    input rocc_cmd_bits_status_dv,
    input [PRV_SZ-1:0] rocc_cmd_bits_status_prv,
    input rocc_cmd_bits_status_v,
    input rocc_cmd_bits_status_sd,
    input [22:0] rocc_cmd_bits_status_zero2,
    input rocc_cmd_bits_status_mpv,
    input rocc_cmd_bits_status_gva,
    input rocc_cmd_bits_status_mbe,
    input rocc_cmd_bits_status_sbe,
    input [1:0] rocc_cmd_bits_status_sxl,
    input [1:0] rocc_cmd_bits_status_uxl,
    input rocc_cmd_bits_status_sd_rv32,
    input [7:0] rocc_cmd_bits_status_zero1,
    input rocc_cmd_bits_status_tsr,
    input rocc_cmd_bits_status_tw,
    input rocc_cmd_bits_status_tvm,
    input rocc_cmd_bits_status_mxr,
    input rocc_cmd_bits_status_sum,
    input rocc_cmd_bits_status_mprv,
    input [1:0] rocc_cmd_bits_status_xs,
    input [1:0] rocc_cmd_bits_status_fs,
    input [1:0] rocc_cmd_bits_status_vs,
    input [1:0] rocc_cmd_bits_status_mpp,
    input [0:0] rocc_cmd_bits_status_spp,
    input rocc_cmd_bits_status_mpie,
    input rocc_cmd_bits_status_ube,
    input rocc_cmd_bits_status_spie,
    input rocc_cmd_bits_status_upie,
    input rocc_cmd_bits_status_mie,
    input rocc_cmd_bits_status_hie,
    input rocc_cmd_bits_status_sie,
    input rocc_cmd_bits_status_uie,
    input rocc_resp_ready,
    output rocc_resp_valid,
    output [4:0] rocc_resp_bits_rd,
    output [xLen-1:0] rocc_resp_bits_data,
    input rocc_mem_req_ready,
    output rocc_mem_req_valid,
    output [coreMaxAddrBits-1:0] rocc_mem_req_bits_addr,
    output [dcacheReqTagBits-1:0] rocc_mem_req_bits_tag,
    output [M_SZ-1:0] rocc_mem_req_bits_cmd,
    output [mem_req_bits_size_width-1:0] rocc_mem_req_bits_size,
    output rocc_mem_req_bits_signed,
    output rocc_mem_req_bits_phys,
    output rocc_mem_req_bits_no_alloc,
    output rocc_mem_req_bits_no_xcpt,
    output [1:0] rocc_mem_req_bits_dprv,
    output rocc_mem_req_bits_dv,
    output [coreDataBits-1:0] rocc_mem_req_bits_data,
    output [coreDataBytes-1:0] rocc_mem_req_bits_mask,
    output rocc_mem_s1_kill,
    output [coreDataBits-1:0] rocc_mem_s1_data_data,
    output [coreDataBytes-1:0] rocc_mem_s1_data_mask,
    input rocc_mem_s2_nack,
    input rocc_mem_s2_nack_cause_raw,
    output rocc_mem_s2_kill,
    input rocc_mem_s2_uncached,
    input [paddrBits-1:0] rocc_mem_s2_paddr,
    input [vaddrBitsExtended-1:0] rocc_mem_s2_gpa,
    input rocc_mem_s2_gpa_is_pte,
    input rocc_mem_resp_valid,
    input [coreMaxAddrBits-1:0] rocc_mem_resp_bits_addr,
    input [dcacheReqTagBits-1:0] rocc_mem_resp_bits_tag,
    input [M_SZ-1:0] rocc_mem_resp_bits_cmd,
    input [mem_req_bits_size_width-1:0] rocc_mem_resp_bits_size,
    input rocc_mem_resp_bits_signed,
    input [coreDataBits-1:0] rocc_mem_resp_bits_data,
    input [coreDataBytes-1:0] rocc_mem_resp_bits_mask,
    input rocc_mem_resp_bits_replay,
    input rocc_mem_resp_bits_has_data,
    input [coreDataBits-1:0] rocc_mem_resp_bits_data_word_bypass,
    input [coreDataBits-1:0] rocc_mem_resp_bits_data_raw,
    input [coreDataBits-1:0] rocc_mem_resp_bits_store_data,
    input [1:0] rocc_mem_resp_bits_dprv,
    input rocc_mem_resp_bits_dv,
    input rocc_mem_replay_next,
    input rocc_mem_s2_xcpt_ma_ld,
    input rocc_mem_s2_xcpt_ma_st,
    input rocc_mem_s2_xcpt_pf_ld,
    input rocc_mem_s2_xcpt_pf_st,
    input rocc_mem_s2_xcpt_gf_ld,
    input rocc_mem_s2_xcpt_gf_st,
    input rocc_mem_s2_xcpt_ae_ld,
    input rocc_mem_s2_xcpt_ae_st,
    input rocc_mem_ordered,
    input rocc_mem_perf_acquire,
    input rocc_mem_perf_release,
    input rocc_mem_perf_grant,
    input rocc_mem_perf_tlbMiss,
    input rocc_mem_perf_blocked,
    input rocc_mem_perf_canAcceptStoreThenLoad,
    input rocc_mem_perf_canAcceptStoreThenRMW,
    input rocc_mem_perf_canAcceptLoadThenLoad,
    input rocc_mem_perf_storeBufferEmptyAfterLoad,
    input rocc_mem_perf_storeBufferEmptyAfterStore,
    output rocc_mem_keep_clock_enabled,
    input rocc_mem_clock_enabled,
    output rocc_busy,
    output rocc_interrupt,
    input rocc_exception,
    input rocc_fpu_req_ready,
    output rocc_fpu_req_valid,
    output rocc_fpu_req_bits_ldst,
    output rocc_fpu_req_bits_wen,
    output rocc_fpu_req_bits_ren1,
    output rocc_fpu_req_bits_ren2,
    output rocc_fpu_req_bits_ren3,
    output rocc_fpu_req_bits_swap12,
    output rocc_fpu_req_bits_swap23,
    output [1:0] rocc_fpu_req_bits_typeTagIn,
    output [1:0] rocc_fpu_req_bits_typeTagOut,
    output rocc_fpu_req_bits_fromint,
    output rocc_fpu_req_bits_toint,
    output rocc_fpu_req_bits_fastpipe,
    output rocc_fpu_req_bits_fma,
    output rocc_fpu_req_bits_div,
    output rocc_fpu_req_bits_sqrt,
    output rocc_fpu_req_bits_wflags,
    output [FPConstants_RM_SZ-1:0] rocc_fpu_req_bits_rm,
    output [1:0] rocc_fpu_req_bits_fmaCmd,
    output [1:0] rocc_fpu_req_bits_typ,
    output [1:0] rocc_fpu_req_bits_fmt,
    output [fLen:0] rocc_fpu_req_bits_in1,
    output [fLen:0] rocc_fpu_req_bits_in2,
    output [fLen:0] rocc_fpu_req_bits_in3,
    output rocc_fpu_resp_ready,
    input rocc_fpu_resp_valid,
    input [fLen:0] rocc_fpu_resp_bits_data,
    input [FPConstants_FLAGS_SZ-1:0] rocc_fpu_resp_bits_exc );

  assign rocc_cmd_ready = 1'b1;

  assign rocc_mem_req_valid = 1'b0;
  assign rocc_mem_s1_kill = 1'b0;
  assign rocc_mem_s2_kill = 1'b0;

  assign rocc_busy = 1'b0;
  assign rocc_interrupt = 1'b0;

  assign rocc_fpu_req_valid = 1'b0;
  assign rocc_fpu_resp_ready = 1'b1;

  /* Accumulate rs1 and rs2 into an accumulator */
  reg [xLen-1:0] acc;
  reg doResp;
  reg [4:0] rocc_cmd_bits_inst_rd_d;
  always @ (posedge clock) begin
    if (reset) begin
      acc <= {xLen{1'b0}};
      doResp <= 1'b0;
      rocc_cmd_bits_inst_rd_d <= 5'b0;
    end
    else if (rocc_cmd_valid && rocc_cmd_ready) begin
      doResp                  <= rocc_cmd_bits_inst_xd;
      rocc_cmd_bits_inst_rd_d <= rocc_cmd_bits_inst_rd;
      acc                     <= acc + rocc_cmd_bits_rs1 + rocc_cmd_bits_rs2;
    end
    else begin
      doResp <= 1'b0;
    end
  end

  assign rocc_resp_valid = doResp;
  assign rocc_resp_bits_rd = rocc_cmd_bits_inst_rd_d;
  assign rocc_resp_bits_data = acc;

endmodule
