///////////////////////////////////////////////////////////////////////////////
//  Copyright (c) 1995/2018 Xilinx, Inc.
//  All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /     Vendor      : Xilinx
// \   \   \/      Version     : 2018.3
//  \   \          Description : Xilinx Unified Simulation Library Component
//  /   /                        ME_PL_M_EVENTS
// /___/   /\      Filename    : ME_PL_M_EVENTS.v
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

module ME_PL_M_EVENTS
`ifdef XIL_TIMING
#(
  parameter LOC = "UNPLACED"
)
`endif
(
  output [15:0] TRIG,

  input CLK
);

// define constants
  localparam MODULE_NAME = "ME_PL_M_EVENTS";
  
`ifdef XIL_XECLIB
  reg glblGSR = 1'b0;
`else
  tri0 glblGSR = glbl.GSR;
`endif

`ifdef XIL_TIMING
  reg notifier;
`endif

// begin behavioral model

// end behavioral model

`ifndef XIL_XECLIB
`ifdef XIL_TIMING
  specify
    (CLK => TRIG[0]) = (0:0:0, 0:0:0);
    (CLK => TRIG[10]) = (0:0:0, 0:0:0);
    (CLK => TRIG[11]) = (0:0:0, 0:0:0);
    (CLK => TRIG[12]) = (0:0:0, 0:0:0);
    (CLK => TRIG[13]) = (0:0:0, 0:0:0);
    (CLK => TRIG[14]) = (0:0:0, 0:0:0);
    (CLK => TRIG[15]) = (0:0:0, 0:0:0);
    (CLK => TRIG[1]) = (0:0:0, 0:0:0);
    (CLK => TRIG[2]) = (0:0:0, 0:0:0);
    (CLK => TRIG[3]) = (0:0:0, 0:0:0);
    (CLK => TRIG[4]) = (0:0:0, 0:0:0);
    (CLK => TRIG[5]) = (0:0:0, 0:0:0);
    (CLK => TRIG[6]) = (0:0:0, 0:0:0);
    (CLK => TRIG[7]) = (0:0:0, 0:0:0);
    (CLK => TRIG[8]) = (0:0:0, 0:0:0);
    (CLK => TRIG[9]) = (0:0:0, 0:0:0);
    $period (negedge CLK, 0:0:0, notifier);
    $period (posedge CLK, 0:0:0, notifier);
    $width (negedge CLK, 0:0:0, 0, notifier);
    $width (posedge CLK, 0:0:0, 0, notifier);
    specparam PATHPULSE$ = 0;
  endspecify
`endif
`endif
endmodule

`endcelldefine
