///////////////////////////////////////////////////////////////////////////////
//  Copyright (c) 1995/2016 Xilinx, Inc.
//  All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /     Vendor      : Xilinx
// \   \   \/      Version     : 2016.4
//  \   \          Description : Xilinx Unified Simulation Library Component
//  /   /                        Fast Carry Logic with Look Ahead
// /___/   /\      Filename    : CARRY8.v
// \   \  /  \
//  \___\/\___\
//
///////////////////////////////////////////////////////////////////////////////
//  Revision
//    09/26/12 - Initial functional version.
//    05/24/13 - Add CARRY_TYPE, CI_TOP
//    10/22/14 - 808642 - Added #1 to $finish
//  End Revision
///////////////////////////////////////////////////////////////////////////////

`timescale 1 ps / 1 ps

`celldefine

module CARRY8 #(
`ifdef XIL_TIMING
    parameter LOC = "UNPLACED",
`endif
    parameter CARRY_TYPE = "SINGLE_CY8"
) (
    output [7:0] CO,
    output [7:0] O,

    input CI,
    input CI_TOP,
    input [7:0] DI,
    input [7:0] S
);

  // the carry chain
  assign CO[7] = S[7] ? CO[6] : DI[7];
  assign CO[6] = S[6] ? CO[5] : DI[6];
  assign CO[5] = S[5] ? CO[4] : DI[5];
  assign CO[4] = S[4] ? CO[3] : DI[4];
  assign CO[3] = S[3] ? CO[2] : DI[3];
  assign CO[2] = S[2] ? CO[1] : DI[2];
  assign CO[1] = S[1] ? CO[0] : DI[1];
  assign CO[0] = S[0] ? CI : DI[0];

  // XOR logic(sum)
  assign O[7]  = S[7] ^ CO[6];
  assign O[6]  = S[6] ^ CO[5];
  assign O[5]  = S[5] ^ CO[4];
  assign O[4]  = S[4] ^ CO[3];
  assign O[3]  = S[3] ^ CO[2];
  assign O[2]  = S[2] ^ CO[1];
  assign O[1]  = S[1] ^ CO[0];
  assign O[0]  = S[0] ^ CI;

endmodule

`endcelldefine
