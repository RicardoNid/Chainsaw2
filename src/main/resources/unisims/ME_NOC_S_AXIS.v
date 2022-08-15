///////////////////////////////////////////////////////////////////////////////
//  Copyright (c) 1995/2018 Xilinx, Inc.
//  All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /     Vendor      : Xilinx
// \   \   \/      Version     : 2018.3
//  \   \          Description : Xilinx Unified Simulation Library Component
//  /   /                        ME_NOC_S_AXIS
// /___/   /\      Filename    : ME_NOC_S_AXIS.v
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

module ME_NOC_S_AXIS
`ifdef XIL_TIMING
#(
  parameter LOC = "UNPLACED"
)
`endif
(
  output S_AXIS_ACLK,
  output S_AXIS_TREADY,

  input [127:0] S_AXIS_TDATA,
  input [9:0] S_AXIS_TDEST,
  input [1:0] S_AXIS_TID,
  input [15:0] S_AXIS_TKEEP,
  input S_AXIS_TLAST,
  input S_AXIS_TVALID
);

// define constants
  localparam MODULE_NAME = "ME_NOC_S_AXIS";
  
`ifdef XIL_XECLIB
  reg glblGSR = 1'b0;
`else
  tri0 glblGSR = glbl.GSR;
`endif

// begin behavioral model

// end behavioral model

endmodule

`endcelldefine
