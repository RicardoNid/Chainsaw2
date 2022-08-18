///////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1995/2016 Xilinx, Inc.
// All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /    Vendor      : Xilinx
// \   \   \/     Version     : 2017.1
//  \   \         Description : Xilinx Unified Simulation Library Component
//  /   /                  6-Bit Look-Up Table with Two Outputs
// /___/   /\     Filename : LUT6_2.v
// \   \  /  \
//  \___\/\___\
//
///////////////////////////////////////////////////////////////////////////////
//  Revision:
//    08/08/06 - Initial version.
//    06/04/07 - Change timescale form 100ps/10ps to 1ps/1ps.
//               Add wire definition.
//    06/19/07 - 441956 - Add LOC Parameter
//    12/13/11 - 524859 - Added `celldefine and `endcelldefine
//    09/12/16 - ANSI ports, speed improvements
//  End Revision:
///////////////////////////////////////////////////////////////////////////////

`timescale 1 ps/1 ps

`celldefine

module LUT6_2 #(
`ifdef XIL_TIMING
    parameter LOC = "UNPLACED",
`endif
    parameter [63:0] INIT = 64'h0000000000000000
) (
    output O5,
    output O6,

    input I0,
    input I1,
    input I2,
    input I3,
    input I4,
    input I5
);

  assign O5 = INIT[{I4, I3, I2, I1, I0}];
  assign O6 = INIT[{I5, I4, I3, I2, I1, I0}];

endmodule

`endcelldefine
