///////////////////////////////////////////////////////////////////////////////
//  Copyright (c) 1995/2018 Xilinx, Inc.
//  All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /     Vendor      : Xilinx
// \   \   \/      Version     : 2019.1
//  \   \          Description : Xilinx Unified Simulation Library Component
//  /   /                        NOC_NPS7575
// /___/   /\      Filename    : NOC_NPS7575.v
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

module NOC_NPS7575 #(
  parameter [31:0] REG_CLOCK_MUX = 32'h00000000,
  parameter [31:0] REG_HIGH_ID0_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID0_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID10_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID10_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID11_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID11_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID12_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID12_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID13_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID13_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID14_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID14_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID15_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID15_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID16_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID16_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID17_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID17_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID18_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID18_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID19_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID19_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID1_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID1_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID20_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID20_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID21_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID21_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID22_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID22_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID23_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID23_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID24_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID24_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID25_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID25_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID26_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID26_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID27_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID27_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID28_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID28_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID29_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID29_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID2_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID2_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID30_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID30_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID31_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID31_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID32_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID32_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID33_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID33_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID34_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID34_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID35_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID35_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID36_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID36_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID37_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID37_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID38_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID38_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID39_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID39_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID3_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID3_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID40_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID40_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID41_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID41_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID42_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID42_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID43_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID43_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID44_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID44_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID45_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID45_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID46_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID46_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID47_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID47_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID48_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID48_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID49_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID49_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID4_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID4_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID50_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID50_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID51_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID51_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID52_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID52_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID53_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID53_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID54_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID54_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID55_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID55_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID56_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID56_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID57_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID57_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID58_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID58_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID59_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID59_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID5_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID5_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID60_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID60_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID61_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID61_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID62_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID62_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID63_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID63_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID6_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID6_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID7_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID7_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID8_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID8_P23 = 32'h55550000,
  parameter [31:0] REG_HIGH_ID9_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_HIGH_ID9_P23 = 32'h55550000,
  parameter [9:0] REG_ID = 10'h000,
  parameter [31:0] REG_LOW_ID0_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID0_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID10_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID10_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID11_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID11_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID12_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID12_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID13_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID13_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID14_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID14_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID15_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID15_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID1_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID1_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID2_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID2_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID3_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID3_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID4_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID4_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID5_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID5_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID6_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID6_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID7_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID7_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID8_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID8_P23 = 32'h55550000,
  parameter [31:0] REG_LOW_ID9_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_LOW_ID9_P23 = 32'h55550000,
  parameter [31:0] REG_MID_ID0_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_MID_ID0_P23 = 32'h55550000,
  parameter [31:0] REG_MID_ID1_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_MID_ID1_P23 = 32'h55550000,
  parameter [31:0] REG_MID_ID2_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_MID_ID2_P23 = 32'h55550000,
  parameter [31:0] REG_MID_ID3_P01 = 32'hFFFFAAAA,
  parameter [31:0] REG_MID_ID3_P23 = 32'h55550000,
  parameter [15:0] REG_NOC_CTL = 16'hFF00,
  parameter [31:0] REG_P00_P1_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P00_P1_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P01_P2_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P01_P2_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P02_P3_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P02_P3_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P10_P2_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P10_P2_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P11_P3_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P11_P3_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P12_P0_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P12_P0_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P20_P3_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P20_P3_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P21_P0_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P21_P0_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P22_P1_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P22_P1_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P30_P0_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P30_P0_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P31_P1_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P31_P1_1_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P32_P2_0_VCA_TOKEN = 32'h10101010,
  parameter [31:0] REG_P32_P2_1_VCA_TOKEN = 32'h10101010
)(
  output [7:0] IF_NOC_NPP_IN0_NOC_CREDIT_RETURN,
  output IF_NOC_NPP_IN0_NOC_CREDIT_RETURN_EN,
  output [7:0] IF_NOC_NPP_IN1_NOC_CREDIT_RETURN,
  output IF_NOC_NPP_IN1_NOC_CREDIT_RETURN_EN,
  output [7:0] IF_NOC_NPP_IN2_NOC_CREDIT_RETURN,
  output IF_NOC_NPP_IN2_NOC_CREDIT_RETURN_EN,
  output [7:0] IF_NOC_NPP_IN3_NOC_CREDIT_RETURN,
  output IF_NOC_NPP_IN3_NOC_CREDIT_RETURN_EN,
  output IF_NOC_NPP_OUT0_NOC_CREDIT_RDY,
  output [181:0] IF_NOC_NPP_OUT0_NOC_FLIT,
  output IF_NOC_NPP_OUT0_NOC_FLIT_EN,
  output [7:0] IF_NOC_NPP_OUT0_NOC_VALID,
  output IF_NOC_NPP_OUT0_NOC_VALID_EN,
  output IF_NOC_NPP_OUT1_NOC_CREDIT_RDY,
  output [181:0] IF_NOC_NPP_OUT1_NOC_FLIT,
  output IF_NOC_NPP_OUT1_NOC_FLIT_EN,
  output [7:0] IF_NOC_NPP_OUT1_NOC_VALID,
  output IF_NOC_NPP_OUT1_NOC_VALID_EN,
  output IF_NOC_NPP_OUT2_NOC_CREDIT_RDY,
  output [181:0] IF_NOC_NPP_OUT2_NOC_FLIT,
  output IF_NOC_NPP_OUT2_NOC_FLIT_EN,
  output [7:0] IF_NOC_NPP_OUT2_NOC_VALID,
  output IF_NOC_NPP_OUT2_NOC_VALID_EN,
  output IF_NOC_NPP_OUT3_NOC_CREDIT_RDY,
  output [181:0] IF_NOC_NPP_OUT3_NOC_FLIT,
  output IF_NOC_NPP_OUT3_NOC_FLIT_EN,
  output [7:0] IF_NOC_NPP_OUT3_NOC_VALID,
  output IF_NOC_NPP_OUT3_NOC_VALID_EN,

  input IF_NOC_NPP_IN0_NOC_CREDIT_RDY,
  input [181:0] IF_NOC_NPP_IN0_NOC_FLIT,
  input IF_NOC_NPP_IN0_NOC_FLIT_EN,
  input [7:0] IF_NOC_NPP_IN0_NOC_VALID,
  input IF_NOC_NPP_IN0_NOC_VALID_EN,
  input IF_NOC_NPP_IN1_NOC_CREDIT_RDY,
  input [181:0] IF_NOC_NPP_IN1_NOC_FLIT,
  input IF_NOC_NPP_IN1_NOC_FLIT_EN,
  input [7:0] IF_NOC_NPP_IN1_NOC_VALID,
  input IF_NOC_NPP_IN1_NOC_VALID_EN,
  input IF_NOC_NPP_IN2_NOC_CREDIT_RDY,
  input [181:0] IF_NOC_NPP_IN2_NOC_FLIT,
  input IF_NOC_NPP_IN2_NOC_FLIT_EN,
  input [7:0] IF_NOC_NPP_IN2_NOC_VALID,
  input IF_NOC_NPP_IN2_NOC_VALID_EN,
  input IF_NOC_NPP_IN3_NOC_CREDIT_RDY,
  input [181:0] IF_NOC_NPP_IN3_NOC_FLIT,
  input IF_NOC_NPP_IN3_NOC_FLIT_EN,
  input [7:0] IF_NOC_NPP_IN3_NOC_VALID,
  input IF_NOC_NPP_IN3_NOC_VALID_EN,
  input [7:0] IF_NOC_NPP_OUT0_NOC_CREDIT_RETURN,
  input IF_NOC_NPP_OUT0_NOC_CREDIT_RETURN_EN,
  input [7:0] IF_NOC_NPP_OUT1_NOC_CREDIT_RETURN,
  input IF_NOC_NPP_OUT1_NOC_CREDIT_RETURN_EN,
  input [7:0] IF_NOC_NPP_OUT2_NOC_CREDIT_RETURN,
  input IF_NOC_NPP_OUT2_NOC_CREDIT_RETURN_EN,
  input [7:0] IF_NOC_NPP_OUT3_NOC_CREDIT_RETURN,
  input IF_NOC_NPP_OUT3_NOC_CREDIT_RETURN_EN
);

// define constants
  localparam MODULE_NAME = "NOC_NPS7575";
  

      BM_NOC_NPS7575 #(
      .REG_CLOCK_MUX (REG_CLOCK_MUX),
      .REG_HIGH_ID0_P01 (REG_HIGH_ID0_P01),
      .REG_HIGH_ID0_P23 (REG_HIGH_ID0_P23),
      .REG_HIGH_ID10_P01 (REG_HIGH_ID10_P01),
      .REG_HIGH_ID10_P23 (REG_HIGH_ID10_P23),
      .REG_HIGH_ID11_P01 (REG_HIGH_ID11_P01),
      .REG_HIGH_ID11_P23 (REG_HIGH_ID11_P23),
      .REG_HIGH_ID12_P01 (REG_HIGH_ID12_P01),
      .REG_HIGH_ID12_P23 (REG_HIGH_ID12_P23),
      .REG_HIGH_ID13_P01 (REG_HIGH_ID13_P01),
      .REG_HIGH_ID13_P23 (REG_HIGH_ID13_P23),
      .REG_HIGH_ID14_P01 (REG_HIGH_ID14_P01),
      .REG_HIGH_ID14_P23 (REG_HIGH_ID14_P23),
      .REG_HIGH_ID15_P01 (REG_HIGH_ID15_P01),
      .REG_HIGH_ID15_P23 (REG_HIGH_ID15_P23),
      .REG_HIGH_ID16_P01 (REG_HIGH_ID16_P01),
      .REG_HIGH_ID16_P23 (REG_HIGH_ID16_P23),
      .REG_HIGH_ID17_P01 (REG_HIGH_ID17_P01),
      .REG_HIGH_ID17_P23 (REG_HIGH_ID17_P23),
      .REG_HIGH_ID18_P01 (REG_HIGH_ID18_P01),
      .REG_HIGH_ID18_P23 (REG_HIGH_ID18_P23),
      .REG_HIGH_ID19_P01 (REG_HIGH_ID19_P01),
      .REG_HIGH_ID19_P23 (REG_HIGH_ID19_P23),
      .REG_HIGH_ID1_P01 (REG_HIGH_ID1_P01),
      .REG_HIGH_ID1_P23 (REG_HIGH_ID1_P23),
      .REG_HIGH_ID20_P01 (REG_HIGH_ID20_P01),
      .REG_HIGH_ID20_P23 (REG_HIGH_ID20_P23),
      .REG_HIGH_ID21_P01 (REG_HIGH_ID21_P01),
      .REG_HIGH_ID21_P23 (REG_HIGH_ID21_P23),
      .REG_HIGH_ID22_P01 (REG_HIGH_ID22_P01),
      .REG_HIGH_ID22_P23 (REG_HIGH_ID22_P23),
      .REG_HIGH_ID23_P01 (REG_HIGH_ID23_P01),
      .REG_HIGH_ID23_P23 (REG_HIGH_ID23_P23),
      .REG_HIGH_ID24_P01 (REG_HIGH_ID24_P01),
      .REG_HIGH_ID24_P23 (REG_HIGH_ID24_P23),
      .REG_HIGH_ID25_P01 (REG_HIGH_ID25_P01),
      .REG_HIGH_ID25_P23 (REG_HIGH_ID25_P23),
      .REG_HIGH_ID26_P01 (REG_HIGH_ID26_P01),
      .REG_HIGH_ID26_P23 (REG_HIGH_ID26_P23),
      .REG_HIGH_ID27_P01 (REG_HIGH_ID27_P01),
      .REG_HIGH_ID27_P23 (REG_HIGH_ID27_P23),
      .REG_HIGH_ID28_P01 (REG_HIGH_ID28_P01),
      .REG_HIGH_ID28_P23 (REG_HIGH_ID28_P23),
      .REG_HIGH_ID29_P01 (REG_HIGH_ID29_P01),
      .REG_HIGH_ID29_P23 (REG_HIGH_ID29_P23),
      .REG_HIGH_ID2_P01 (REG_HIGH_ID2_P01),
      .REG_HIGH_ID2_P23 (REG_HIGH_ID2_P23),
      .REG_HIGH_ID30_P01 (REG_HIGH_ID30_P01),
      .REG_HIGH_ID30_P23 (REG_HIGH_ID30_P23),
      .REG_HIGH_ID31_P01 (REG_HIGH_ID31_P01),
      .REG_HIGH_ID31_P23 (REG_HIGH_ID31_P23),
      .REG_HIGH_ID32_P01 (REG_HIGH_ID32_P01),
      .REG_HIGH_ID32_P23 (REG_HIGH_ID32_P23),
      .REG_HIGH_ID33_P01 (REG_HIGH_ID33_P01),
      .REG_HIGH_ID33_P23 (REG_HIGH_ID33_P23),
      .REG_HIGH_ID34_P01 (REG_HIGH_ID34_P01),
      .REG_HIGH_ID34_P23 (REG_HIGH_ID34_P23),
      .REG_HIGH_ID35_P01 (REG_HIGH_ID35_P01),
      .REG_HIGH_ID35_P23 (REG_HIGH_ID35_P23),
      .REG_HIGH_ID36_P01 (REG_HIGH_ID36_P01),
      .REG_HIGH_ID36_P23 (REG_HIGH_ID36_P23),
      .REG_HIGH_ID37_P01 (REG_HIGH_ID37_P01),
      .REG_HIGH_ID37_P23 (REG_HIGH_ID37_P23),
      .REG_HIGH_ID38_P01 (REG_HIGH_ID38_P01),
      .REG_HIGH_ID38_P23 (REG_HIGH_ID38_P23),
      .REG_HIGH_ID39_P01 (REG_HIGH_ID39_P01),
      .REG_HIGH_ID39_P23 (REG_HIGH_ID39_P23),
      .REG_HIGH_ID3_P01 (REG_HIGH_ID3_P01),
      .REG_HIGH_ID3_P23 (REG_HIGH_ID3_P23),
      .REG_HIGH_ID40_P01 (REG_HIGH_ID40_P01),
      .REG_HIGH_ID40_P23 (REG_HIGH_ID40_P23),
      .REG_HIGH_ID41_P01 (REG_HIGH_ID41_P01),
      .REG_HIGH_ID41_P23 (REG_HIGH_ID41_P23),
      .REG_HIGH_ID42_P01 (REG_HIGH_ID42_P01),
      .REG_HIGH_ID42_P23 (REG_HIGH_ID42_P23),
      .REG_HIGH_ID43_P01 (REG_HIGH_ID43_P01),
      .REG_HIGH_ID43_P23 (REG_HIGH_ID43_P23),
      .REG_HIGH_ID44_P01 (REG_HIGH_ID44_P01),
      .REG_HIGH_ID44_P23 (REG_HIGH_ID44_P23),
      .REG_HIGH_ID45_P01 (REG_HIGH_ID45_P01),
      .REG_HIGH_ID45_P23 (REG_HIGH_ID45_P23),
      .REG_HIGH_ID46_P01 (REG_HIGH_ID46_P01),
      .REG_HIGH_ID46_P23 (REG_HIGH_ID46_P23),
      .REG_HIGH_ID47_P01 (REG_HIGH_ID47_P01),
      .REG_HIGH_ID47_P23 (REG_HIGH_ID47_P23),
      .REG_HIGH_ID48_P01 (REG_HIGH_ID48_P01),
      .REG_HIGH_ID48_P23 (REG_HIGH_ID48_P23),
      .REG_HIGH_ID49_P01 (REG_HIGH_ID49_P01),
      .REG_HIGH_ID49_P23 (REG_HIGH_ID49_P23),
      .REG_HIGH_ID4_P01 (REG_HIGH_ID4_P01),
      .REG_HIGH_ID4_P23 (REG_HIGH_ID4_P23),
      .REG_HIGH_ID50_P01 (REG_HIGH_ID50_P01),
      .REG_HIGH_ID50_P23 (REG_HIGH_ID50_P23),
      .REG_HIGH_ID51_P01 (REG_HIGH_ID51_P01),
      .REG_HIGH_ID51_P23 (REG_HIGH_ID51_P23),
      .REG_HIGH_ID52_P01 (REG_HIGH_ID52_P01),
      .REG_HIGH_ID52_P23 (REG_HIGH_ID52_P23),
      .REG_HIGH_ID53_P01 (REG_HIGH_ID53_P01),
      .REG_HIGH_ID53_P23 (REG_HIGH_ID53_P23),
      .REG_HIGH_ID54_P01 (REG_HIGH_ID54_P01),
      .REG_HIGH_ID54_P23 (REG_HIGH_ID54_P23),
      .REG_HIGH_ID55_P01 (REG_HIGH_ID55_P01),
      .REG_HIGH_ID55_P23 (REG_HIGH_ID55_P23),
      .REG_HIGH_ID56_P01 (REG_HIGH_ID56_P01),
      .REG_HIGH_ID56_P23 (REG_HIGH_ID56_P23),
      .REG_HIGH_ID57_P01 (REG_HIGH_ID57_P01),
      .REG_HIGH_ID57_P23 (REG_HIGH_ID57_P23),
      .REG_HIGH_ID58_P01 (REG_HIGH_ID58_P01),
      .REG_HIGH_ID58_P23 (REG_HIGH_ID58_P23),
      .REG_HIGH_ID59_P01 (REG_HIGH_ID59_P01),
      .REG_HIGH_ID59_P23 (REG_HIGH_ID59_P23),
      .REG_HIGH_ID5_P01 (REG_HIGH_ID5_P01),
      .REG_HIGH_ID5_P23 (REG_HIGH_ID5_P23),
      .REG_HIGH_ID60_P01 (REG_HIGH_ID60_P01),
      .REG_HIGH_ID60_P23 (REG_HIGH_ID60_P23),
      .REG_HIGH_ID61_P01 (REG_HIGH_ID61_P01),
      .REG_HIGH_ID61_P23 (REG_HIGH_ID61_P23),
      .REG_HIGH_ID62_P01 (REG_HIGH_ID62_P01),
      .REG_HIGH_ID62_P23 (REG_HIGH_ID62_P23),
      .REG_HIGH_ID63_P01 (REG_HIGH_ID63_P01),
      .REG_HIGH_ID63_P23 (REG_HIGH_ID63_P23),
      .REG_HIGH_ID6_P01 (REG_HIGH_ID6_P01),
      .REG_HIGH_ID6_P23 (REG_HIGH_ID6_P23),
      .REG_HIGH_ID7_P01 (REG_HIGH_ID7_P01),
      .REG_HIGH_ID7_P23 (REG_HIGH_ID7_P23),
      .REG_HIGH_ID8_P01 (REG_HIGH_ID8_P01),
      .REG_HIGH_ID8_P23 (REG_HIGH_ID8_P23),
      .REG_HIGH_ID9_P01 (REG_HIGH_ID9_P01),
      .REG_HIGH_ID9_P23 (REG_HIGH_ID9_P23),
      .REG_ID (REG_ID),
      .REG_LOW_ID0_P01 (REG_LOW_ID0_P01),
      .REG_LOW_ID0_P23 (REG_LOW_ID0_P23),
      .REG_LOW_ID10_P01 (REG_LOW_ID10_P01),
      .REG_LOW_ID10_P23 (REG_LOW_ID10_P23),
      .REG_LOW_ID11_P01 (REG_LOW_ID11_P01),
      .REG_LOW_ID11_P23 (REG_LOW_ID11_P23),
      .REG_LOW_ID12_P01 (REG_LOW_ID12_P01),
      .REG_LOW_ID12_P23 (REG_LOW_ID12_P23),
      .REG_LOW_ID13_P01 (REG_LOW_ID13_P01),
      .REG_LOW_ID13_P23 (REG_LOW_ID13_P23),
      .REG_LOW_ID14_P01 (REG_LOW_ID14_P01),
      .REG_LOW_ID14_P23 (REG_LOW_ID14_P23),
      .REG_LOW_ID15_P01 (REG_LOW_ID15_P01),
      .REG_LOW_ID15_P23 (REG_LOW_ID15_P23),
      .REG_LOW_ID1_P01 (REG_LOW_ID1_P01),
      .REG_LOW_ID1_P23 (REG_LOW_ID1_P23),
      .REG_LOW_ID2_P01 (REG_LOW_ID2_P01),
      .REG_LOW_ID2_P23 (REG_LOW_ID2_P23),
      .REG_LOW_ID3_P01 (REG_LOW_ID3_P01),
      .REG_LOW_ID3_P23 (REG_LOW_ID3_P23),
      .REG_LOW_ID4_P01 (REG_LOW_ID4_P01),
      .REG_LOW_ID4_P23 (REG_LOW_ID4_P23),
      .REG_LOW_ID5_P01 (REG_LOW_ID5_P01),
      .REG_LOW_ID5_P23 (REG_LOW_ID5_P23),
      .REG_LOW_ID6_P01 (REG_LOW_ID6_P01),
      .REG_LOW_ID6_P23 (REG_LOW_ID6_P23),
      .REG_LOW_ID7_P01 (REG_LOW_ID7_P01),
      .REG_LOW_ID7_P23 (REG_LOW_ID7_P23),
      .REG_LOW_ID8_P01 (REG_LOW_ID8_P01),
      .REG_LOW_ID8_P23 (REG_LOW_ID8_P23),
      .REG_LOW_ID9_P01 (REG_LOW_ID9_P01),
      .REG_LOW_ID9_P23 (REG_LOW_ID9_P23),
      .REG_MID_ID0_P01 (REG_MID_ID0_P01),
      .REG_MID_ID0_P23 (REG_MID_ID0_P23),
      .REG_MID_ID1_P01 (REG_MID_ID1_P01),
      .REG_MID_ID1_P23 (REG_MID_ID1_P23),
      .REG_MID_ID2_P01 (REG_MID_ID2_P01),
      .REG_MID_ID2_P23 (REG_MID_ID2_P23),
      .REG_MID_ID3_P01 (REG_MID_ID3_P01),
      .REG_MID_ID3_P23 (REG_MID_ID3_P23),
      .REG_NOC_CTL (REG_NOC_CTL),
      .REG_P00_P1_0_VCA_TOKEN (REG_P00_P1_0_VCA_TOKEN),
      .REG_P00_P1_1_VCA_TOKEN (REG_P00_P1_1_VCA_TOKEN),
      .REG_P01_P2_0_VCA_TOKEN (REG_P01_P2_0_VCA_TOKEN),
      .REG_P01_P2_1_VCA_TOKEN (REG_P01_P2_1_VCA_TOKEN),
      .REG_P02_P3_0_VCA_TOKEN (REG_P02_P3_0_VCA_TOKEN),
      .REG_P02_P3_1_VCA_TOKEN (REG_P02_P3_1_VCA_TOKEN),
      .REG_P10_P2_0_VCA_TOKEN (REG_P10_P2_0_VCA_TOKEN),
      .REG_P10_P2_1_VCA_TOKEN (REG_P10_P2_1_VCA_TOKEN),
      .REG_P11_P3_0_VCA_TOKEN (REG_P11_P3_0_VCA_TOKEN),
      .REG_P11_P3_1_VCA_TOKEN (REG_P11_P3_1_VCA_TOKEN),
      .REG_P12_P0_0_VCA_TOKEN (REG_P12_P0_0_VCA_TOKEN),
      .REG_P12_P0_1_VCA_TOKEN (REG_P12_P0_1_VCA_TOKEN),
      .REG_P20_P3_0_VCA_TOKEN (REG_P20_P3_0_VCA_TOKEN),
      .REG_P20_P3_1_VCA_TOKEN (REG_P20_P3_1_VCA_TOKEN),
      .REG_P21_P0_0_VCA_TOKEN (REG_P21_P0_0_VCA_TOKEN),
      .REG_P21_P0_1_VCA_TOKEN (REG_P21_P0_1_VCA_TOKEN),
      .REG_P22_P1_0_VCA_TOKEN (REG_P22_P1_0_VCA_TOKEN),
      .REG_P22_P1_1_VCA_TOKEN (REG_P22_P1_1_VCA_TOKEN),
      .REG_P30_P0_0_VCA_TOKEN (REG_P30_P0_0_VCA_TOKEN),
      .REG_P30_P0_1_VCA_TOKEN (REG_P30_P0_1_VCA_TOKEN),
      .REG_P31_P1_0_VCA_TOKEN (REG_P31_P1_0_VCA_TOKEN),
      .REG_P31_P1_1_VCA_TOKEN (REG_P31_P1_1_VCA_TOKEN),
      .REG_P32_P2_0_VCA_TOKEN (REG_P32_P2_0_VCA_TOKEN),
      .REG_P32_P2_1_VCA_TOKEN (REG_P32_P2_1_VCA_TOKEN)
) BM_NOC_NPS7575_INST (
      .IF_NOC_NPP_IN0_NOC_CREDIT_RETURN (IF_NOC_NPP_IN0_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_IN0_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_IN0_NOC_CREDIT_RETURN_EN),
      .IF_NOC_NPP_IN1_NOC_CREDIT_RETURN (IF_NOC_NPP_IN1_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_IN1_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_IN1_NOC_CREDIT_RETURN_EN),
      .IF_NOC_NPP_IN2_NOC_CREDIT_RETURN (IF_NOC_NPP_IN2_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_IN2_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_IN2_NOC_CREDIT_RETURN_EN),
      .IF_NOC_NPP_IN3_NOC_CREDIT_RETURN (IF_NOC_NPP_IN3_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_IN3_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_IN3_NOC_CREDIT_RETURN_EN),
      .IF_NOC_NPP_OUT0_NOC_CREDIT_RDY (IF_NOC_NPP_OUT0_NOC_CREDIT_RDY),
      .IF_NOC_NPP_OUT0_NOC_FLIT (IF_NOC_NPP_OUT0_NOC_FLIT),
      .IF_NOC_NPP_OUT0_NOC_FLIT_EN (IF_NOC_NPP_OUT0_NOC_FLIT_EN),
      .IF_NOC_NPP_OUT0_NOC_VALID (IF_NOC_NPP_OUT0_NOC_VALID),
      .IF_NOC_NPP_OUT0_NOC_VALID_EN (IF_NOC_NPP_OUT0_NOC_VALID_EN),
      .IF_NOC_NPP_OUT1_NOC_CREDIT_RDY (IF_NOC_NPP_OUT1_NOC_CREDIT_RDY),
      .IF_NOC_NPP_OUT1_NOC_FLIT (IF_NOC_NPP_OUT1_NOC_FLIT),
      .IF_NOC_NPP_OUT1_NOC_FLIT_EN (IF_NOC_NPP_OUT1_NOC_FLIT_EN),
      .IF_NOC_NPP_OUT1_NOC_VALID (IF_NOC_NPP_OUT1_NOC_VALID),
      .IF_NOC_NPP_OUT1_NOC_VALID_EN (IF_NOC_NPP_OUT1_NOC_VALID_EN),
      .IF_NOC_NPP_OUT2_NOC_CREDIT_RDY (IF_NOC_NPP_OUT2_NOC_CREDIT_RDY),
      .IF_NOC_NPP_OUT2_NOC_FLIT (IF_NOC_NPP_OUT2_NOC_FLIT),
      .IF_NOC_NPP_OUT2_NOC_FLIT_EN (IF_NOC_NPP_OUT2_NOC_FLIT_EN),
      .IF_NOC_NPP_OUT2_NOC_VALID (IF_NOC_NPP_OUT2_NOC_VALID),
      .IF_NOC_NPP_OUT2_NOC_VALID_EN (IF_NOC_NPP_OUT2_NOC_VALID_EN),
      .IF_NOC_NPP_OUT3_NOC_CREDIT_RDY (IF_NOC_NPP_OUT3_NOC_CREDIT_RDY),
      .IF_NOC_NPP_OUT3_NOC_FLIT (IF_NOC_NPP_OUT3_NOC_FLIT),
      .IF_NOC_NPP_OUT3_NOC_FLIT_EN (IF_NOC_NPP_OUT3_NOC_FLIT_EN),
      .IF_NOC_NPP_OUT3_NOC_VALID (IF_NOC_NPP_OUT3_NOC_VALID),
      .IF_NOC_NPP_OUT3_NOC_VALID_EN (IF_NOC_NPP_OUT3_NOC_VALID_EN),
      .IF_NOC_NPP_IN0_NOC_CREDIT_RDY (IF_NOC_NPP_IN0_NOC_CREDIT_RDY),
      .IF_NOC_NPP_IN0_NOC_FLIT (IF_NOC_NPP_IN0_NOC_FLIT),
      .IF_NOC_NPP_IN0_NOC_FLIT_EN (IF_NOC_NPP_IN0_NOC_FLIT_EN),
      .IF_NOC_NPP_IN0_NOC_VALID (IF_NOC_NPP_IN0_NOC_VALID),
      .IF_NOC_NPP_IN0_NOC_VALID_EN (IF_NOC_NPP_IN0_NOC_VALID_EN),
      .IF_NOC_NPP_IN1_NOC_CREDIT_RDY (IF_NOC_NPP_IN1_NOC_CREDIT_RDY),
      .IF_NOC_NPP_IN1_NOC_FLIT (IF_NOC_NPP_IN1_NOC_FLIT),
      .IF_NOC_NPP_IN1_NOC_FLIT_EN (IF_NOC_NPP_IN1_NOC_FLIT_EN),
      .IF_NOC_NPP_IN1_NOC_VALID (IF_NOC_NPP_IN1_NOC_VALID),
      .IF_NOC_NPP_IN1_NOC_VALID_EN (IF_NOC_NPP_IN1_NOC_VALID_EN),
      .IF_NOC_NPP_IN2_NOC_CREDIT_RDY (IF_NOC_NPP_IN2_NOC_CREDIT_RDY),
      .IF_NOC_NPP_IN2_NOC_FLIT (IF_NOC_NPP_IN2_NOC_FLIT),
      .IF_NOC_NPP_IN2_NOC_FLIT_EN (IF_NOC_NPP_IN2_NOC_FLIT_EN),
      .IF_NOC_NPP_IN2_NOC_VALID (IF_NOC_NPP_IN2_NOC_VALID),
      .IF_NOC_NPP_IN2_NOC_VALID_EN (IF_NOC_NPP_IN2_NOC_VALID_EN),
      .IF_NOC_NPP_IN3_NOC_CREDIT_RDY (IF_NOC_NPP_IN3_NOC_CREDIT_RDY),
      .IF_NOC_NPP_IN3_NOC_FLIT (IF_NOC_NPP_IN3_NOC_FLIT),
      .IF_NOC_NPP_IN3_NOC_FLIT_EN (IF_NOC_NPP_IN3_NOC_FLIT_EN),
      .IF_NOC_NPP_IN3_NOC_VALID (IF_NOC_NPP_IN3_NOC_VALID),
      .IF_NOC_NPP_IN3_NOC_VALID_EN (IF_NOC_NPP_IN3_NOC_VALID_EN),
      .IF_NOC_NPP_OUT0_NOC_CREDIT_RETURN (IF_NOC_NPP_OUT0_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_OUT0_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_OUT0_NOC_CREDIT_RETURN_EN),
      .IF_NOC_NPP_OUT1_NOC_CREDIT_RETURN (IF_NOC_NPP_OUT1_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_OUT1_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_OUT1_NOC_CREDIT_RETURN_EN),
      .IF_NOC_NPP_OUT2_NOC_CREDIT_RETURN (IF_NOC_NPP_OUT2_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_OUT2_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_OUT2_NOC_CREDIT_RETURN_EN),
      .IF_NOC_NPP_OUT3_NOC_CREDIT_RETURN (IF_NOC_NPP_OUT3_NOC_CREDIT_RETURN),
      .IF_NOC_NPP_OUT3_NOC_CREDIT_RETURN_EN (IF_NOC_NPP_OUT3_NOC_CREDIT_RETURN_EN)
    );
// begin behavioral model

// end behavioral model

endmodule

`endcelldefine
