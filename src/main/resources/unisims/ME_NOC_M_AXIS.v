///////////////////////////////////////////////////////////////////////////////
//  Copyright (c) 1995/2018 Xilinx, Inc.
//  All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /     Vendor      : Xilinx
// \   \   \/      Version     : 2018.3
//  \   \          Description : Xilinx Unified Simulation Library Component
//  /   /                        ME_NOC_M_AXIS
// /___/   /\      Filename    : ME_NOC_M_AXIS.v
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

module ME_NOC_M_AXIS
`ifdef XIL_TIMING
#(
  parameter LOC = "UNPLACED"
)
`endif
(
  output M_AXIS_ACLK,
  output [127:0] M_AXIS_TDATA,
  output [9:0] M_AXIS_TDEST,
  output [1:0] M_AXIS_TID,
  output [15:0] M_AXIS_TKEEP,
  output M_AXIS_TLAST,
  output M_AXIS_TVALID,

  input M_AXIS_TREADY
);

// define constants
  localparam MODULE_NAME = "ME_NOC_M_AXIS";
  
`ifdef XIL_XECLIB
  reg glblGSR = 1'b0;
`else
  tri0 glblGSR = glbl.GSR;
`endif

// begin behavioral model

// end behavioral model

endmodule

`endcelldefine
