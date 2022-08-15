///////////////////////////////////////////////////////////////////////////////
//  Copyright (c) 1995/2019 Xilinx, Inc.
//  All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /     Vendor      : Xilinx
// \   \   \/      Version     : 2020.1
//  \   \          Description : Xilinx Unified Simulation Library Component
//  /   /                        NOC_NIDB
// /___/   /\      Filename    : NOC_NIDB.v
// \   \  /  \
//  \___\/\___\
//
///////////////////////////////////////////////////////////////////////////////
//  Revision:
//
//  End Revision:
///////////////////////////////////////////////////////////////////////////////

`timescale 1 ps / 1 ps

`celldefine

module NOC_NIDB #(
  parameter [31:0] REG_ADDR_REMAP0 = 32'h0000F210,
  parameter [15:0] REG_ADDR_REMAP1 = 16'h0000,
  parameter [31:0] REG_ADDR_REMAP_MASK0 = 32'hFFFFFFFF,
  parameter [15:0] REG_ADDR_REMAP_MASK1 = 16'hFFFF,
  parameter [1:0] REG_BYPASS_CNTL = 2'h0,
  parameter [9:0] REG_DCC_CNTR = 10'h064,
  parameter [1:0] REG_ECC_CHK_EN = 2'h3,
  parameter [2:0] REG_ERR_PKT_DROP_DIS = 3'h0,
  parameter [10:0] REG_IO_CHAR_EN = 11'h000,
  parameter [10:0] REG_IO_CHAR_OUTPUT_MUX_SEL = 11'h000,
  parameter [10:0] REG_IO_CNTRL = 11'h01F,
  parameter [2:0] REG_LOOPBACK_CNTL = 3'h0,
  parameter [15:0] REG_NOC_CTL = 16'hFF00,
  parameter [31:0] REG_P0_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P0_1_VCA_TOKEN = 32'h10101010,
  parameter [9:0] REG_RX_DELAY_EN = 10'h000,
  parameter [17:0] REG_RX_DW0_DELAY = 18'h00603,
  parameter [17:0] REG_RX_DW1_DELAY = 18'h00603,
  parameter [17:0] REG_RX_DW2_DELAY = 18'h00603,
  parameter [17:0] REG_RX_DW3_DELAY = 18'h00603,
  parameter [17:0] REG_RX_DW4_DELAY = 18'h00603,
  parameter [7:0] REG_TX_DCC_DELAY = 8'h00,
  parameter [5:0] REG_VC0_ARPROT_SEL = 6'h00,
  parameter [19:0] REG_VC0_SMID_SEL = 20'h00000,
  parameter [5:0] REG_VC1_AWPROT_SEL = 6'h00,
  parameter [19:0] REG_VC1_SMID_SEL = 20'h00000,
  parameter [5:0] REG_VC4_ARPROT_SEL = 6'h00,
  parameter [19:0] REG_VC4_SMID_SEL = 20'h00000,
  parameter [5:0] REG_VC5_AWPROT_SEL = 6'h00,
  parameter [19:0] REG_VC5_SMID_SEL = 20'h00000
)(
  output [7:0] IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RETURN,
  output IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RETURN_EN,
  output IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RDY,
  output [181:0] IF_NOC_NPP_OUT_NIDB_NOC_FLIT,
  output IF_NOC_NPP_OUT_NIDB_NOC_FLIT_EN,
  output [7:0] IF_NOC_NPP_OUT_NIDB_NOC_VALID,
  output IF_NOC_NPP_OUT_NIDB_NOC_VALID_EN,
  output [37:0] NIDB_P0_TX_DW0_DATA,
  output [4:0] NIDB_P0_TX_DW0_DBI_REDUN,
  output NIDB_P0_TX_DW0_STRB,
  output [37:0] NIDB_P0_TX_DW1_DATA,
  output [4:0] NIDB_P0_TX_DW1_DBI_REDUN,
  output NIDB_P0_TX_DW1_STRB,
  output [37:0] NIDB_P0_TX_DW2_DATA,
  output [4:0] NIDB_P0_TX_DW2_DBI_REDUN,
  output NIDB_P0_TX_DW2_STRB,
  output [37:0] NIDB_P0_TX_DW3_DATA,
  output [4:0] NIDB_P0_TX_DW3_DBI_REDUN,
  output NIDB_P0_TX_DW3_STRB,
  output [37:0] NIDB_P0_TX_DW4_DATA,
  output [4:0] NIDB_P0_TX_DW4_DBI_REDUN,
  output NIDB_P0_TX_DW4_STRB,

  input IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RDY,
  input [181:0] IF_NOC_NPP_IN_NIDB_NOC_FLIT,
  input IF_NOC_NPP_IN_NIDB_NOC_FLIT_EN,
  input [7:0] IF_NOC_NPP_IN_NIDB_NOC_VALID,
  input IF_NOC_NPP_IN_NIDB_NOC_VALID_EN,
  input [7:0] IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RETURN,
  input IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RETURN_EN,
  input [37:0] NIDB_P0_RX_DW0_DATA,
  input [4:0] NIDB_P0_RX_DW0_DBI_REDUN,
  input NIDB_P0_RX_DW0_STRB,
  input [37:0] NIDB_P0_RX_DW1_DATA,
  input [4:0] NIDB_P0_RX_DW1_DBI_REDUN,
  input NIDB_P0_RX_DW1_STRB,
  input [37:0] NIDB_P0_RX_DW2_DATA,
  input [4:0] NIDB_P0_RX_DW2_DBI_REDUN,
  input NIDB_P0_RX_DW2_STRB,
  input [37:0] NIDB_P0_RX_DW3_DATA,
  input [4:0] NIDB_P0_RX_DW3_DBI_REDUN,
  input NIDB_P0_RX_DW3_STRB,
  input [37:0] NIDB_P0_RX_DW4_DATA,
  input [4:0] NIDB_P0_RX_DW4_DBI_REDUN,
  input NIDB_P0_RX_DW4_STRB
);

// define constants
  localparam MODULE_NAME = "NOC_NIDB";
  

      BM_NOC_NIDB #(
      .REG_ADDR_REMAP0 (REG_ADDR_REMAP0),
      .REG_ADDR_REMAP1 (REG_ADDR_REMAP1),
      .REG_ADDR_REMAP_MASK0 (REG_ADDR_REMAP_MASK0),
      .REG_ADDR_REMAP_MASK1 (REG_ADDR_REMAP_MASK1),
      .REG_BYPASS_CNTL (REG_BYPASS_CNTL),
      .REG_DCC_CNTR (REG_DCC_CNTR),
      .REG_ECC_CHK_EN (REG_ECC_CHK_EN),
      .REG_ERR_PKT_DROP_DIS (REG_ERR_PKT_DROP_DIS),
      .REG_IO_CHAR_EN (REG_IO_CHAR_EN),
      .REG_IO_CHAR_OUTPUT_MUX_SEL (REG_IO_CHAR_OUTPUT_MUX_SEL),
      .REG_IO_CNTRL (REG_IO_CNTRL),
      .REG_LOOPBACK_CNTL (REG_LOOPBACK_CNTL),
      .REG_NOC_CTL (REG_NOC_CTL),
      .REG_P0_0_VCA_TOKEN (REG_P0_0_VCA_TOKEN),
      .REG_P0_1_VCA_TOKEN (REG_P0_1_VCA_TOKEN),
      .REG_RX_DELAY_EN (REG_RX_DELAY_EN),
      .REG_RX_DW0_DELAY (REG_RX_DW0_DELAY),
      .REG_RX_DW1_DELAY (REG_RX_DW1_DELAY),
      .REG_RX_DW2_DELAY (REG_RX_DW2_DELAY),
      .REG_RX_DW3_DELAY (REG_RX_DW3_DELAY),
      .REG_RX_DW4_DELAY (REG_RX_DW4_DELAY),
      .REG_TX_DCC_DELAY (REG_TX_DCC_DELAY),
      .REG_VC0_ARPROT_SEL (REG_VC0_ARPROT_SEL),
      .REG_VC0_SMID_SEL (REG_VC0_SMID_SEL),
      .REG_VC1_AWPROT_SEL (REG_VC1_AWPROT_SEL),
      .REG_VC1_SMID_SEL (REG_VC1_SMID_SEL),
      .REG_VC4_ARPROT_SEL (REG_VC4_ARPROT_SEL),
      .REG_VC4_SMID_SEL (REG_VC4_SMID_SEL),
      .REG_VC5_AWPROT_SEL (REG_VC5_AWPROT_SEL),
      .REG_VC5_SMID_SEL (REG_VC5_SMID_SEL)
) BM_NOC_NIDB_INST (
      .IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RETURN (IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RETURN_EN),
      .IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RDY (IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RDY),
      .IF_NOC_NPP_OUT_NIDB_NOC_FLIT (IF_NOC_NPP_OUT_NIDB_NOC_FLIT),
      .IF_NOC_NPP_OUT_NIDB_NOC_FLIT_EN (IF_NOC_NPP_OUT_NIDB_NOC_FLIT_EN),
      .IF_NOC_NPP_OUT_NIDB_NOC_VALID (IF_NOC_NPP_OUT_NIDB_NOC_VALID),
      .IF_NOC_NPP_OUT_NIDB_NOC_VALID_EN (IF_NOC_NPP_OUT_NIDB_NOC_VALID_EN),
      .NIDB_P0_TX_DW0_DATA (NIDB_P0_TX_DW0_DATA),
      .NIDB_P0_TX_DW0_DBI_REDUN (NIDB_P0_TX_DW0_DBI_REDUN),
      .NIDB_P0_TX_DW0_STRB (NIDB_P0_TX_DW0_STRB),
      .NIDB_P0_TX_DW1_DATA (NIDB_P0_TX_DW1_DATA),
      .NIDB_P0_TX_DW1_DBI_REDUN (NIDB_P0_TX_DW1_DBI_REDUN),
      .NIDB_P0_TX_DW1_STRB (NIDB_P0_TX_DW1_STRB),
      .NIDB_P0_TX_DW2_DATA (NIDB_P0_TX_DW2_DATA),
      .NIDB_P0_TX_DW2_DBI_REDUN (NIDB_P0_TX_DW2_DBI_REDUN),
      .NIDB_P0_TX_DW2_STRB (NIDB_P0_TX_DW2_STRB),
      .NIDB_P0_TX_DW3_DATA (NIDB_P0_TX_DW3_DATA),
      .NIDB_P0_TX_DW3_DBI_REDUN (NIDB_P0_TX_DW3_DBI_REDUN),
      .NIDB_P0_TX_DW3_STRB (NIDB_P0_TX_DW3_STRB),
      .NIDB_P0_TX_DW4_DATA (NIDB_P0_TX_DW4_DATA),
      .NIDB_P0_TX_DW4_DBI_REDUN (NIDB_P0_TX_DW4_DBI_REDUN),
      .NIDB_P0_TX_DW4_STRB (NIDB_P0_TX_DW4_STRB),
      .IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RDY (IF_NOC_NPP_IN_NIDB_NOC_CREDIT_RDY),
      .IF_NOC_NPP_IN_NIDB_NOC_FLIT (IF_NOC_NPP_IN_NIDB_NOC_FLIT),
      .IF_NOC_NPP_IN_NIDB_NOC_FLIT_EN (IF_NOC_NPP_IN_NIDB_NOC_FLIT_EN),
      .IF_NOC_NPP_IN_NIDB_NOC_VALID (IF_NOC_NPP_IN_NIDB_NOC_VALID),
      .IF_NOC_NPP_IN_NIDB_NOC_VALID_EN (IF_NOC_NPP_IN_NIDB_NOC_VALID_EN),
      .IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RETURN (IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_OUT_NIDB_NOC_CREDIT_RETURN_EN),
      .NIDB_P0_RX_DW0_DATA (NIDB_P0_RX_DW0_DATA),
      .NIDB_P0_RX_DW0_DBI_REDUN (NIDB_P0_RX_DW0_DBI_REDUN),
      .NIDB_P0_RX_DW0_STRB (NIDB_P0_RX_DW0_STRB),
      .NIDB_P0_RX_DW1_DATA (NIDB_P0_RX_DW1_DATA),
      .NIDB_P0_RX_DW1_DBI_REDUN (NIDB_P0_RX_DW1_DBI_REDUN),
      .NIDB_P0_RX_DW1_STRB (NIDB_P0_RX_DW1_STRB),
      .NIDB_P0_RX_DW2_DATA (NIDB_P0_RX_DW2_DATA),
      .NIDB_P0_RX_DW2_DBI_REDUN (NIDB_P0_RX_DW2_DBI_REDUN),
      .NIDB_P0_RX_DW2_STRB (NIDB_P0_RX_DW2_STRB),
      .NIDB_P0_RX_DW3_DATA (NIDB_P0_RX_DW3_DATA),
      .NIDB_P0_RX_DW3_DBI_REDUN (NIDB_P0_RX_DW3_DBI_REDUN),
      .NIDB_P0_RX_DW3_STRB (NIDB_P0_RX_DW3_STRB),
      .NIDB_P0_RX_DW4_DATA (NIDB_P0_RX_DW4_DATA),
      .NIDB_P0_RX_DW4_DBI_REDUN (NIDB_P0_RX_DW4_DBI_REDUN),
      .NIDB_P0_RX_DW4_STRB (NIDB_P0_RX_DW4_STRB)
    );
// begin behavioral model

// end behavioral model

endmodule

`endcelldefine
