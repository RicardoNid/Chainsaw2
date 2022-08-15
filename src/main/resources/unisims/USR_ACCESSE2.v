///////////////////////////////////////////////////////
//  Copyright (c) 2009 Xilinx Inc.
//  All Right Reserved.
///////////////////////////////////////////////////////
//
//   ____   ___
//  /   /\/   / 
// /___/  \  /     Vendor      : Xilinx 
// \  \    \/      Version     :  12.1
//  \  \           Description : 
//  /  /                      
// /__/   /\       Filename    : USR_ACCESSE2.v
// \  \  /  \ 
//  \__\/\__ \                    
//                                 
//  Revision:		1.0
///////////////////////////////////////////////////////

`timescale 1 ps / 1 ps 

`celldefine

module USR_ACCESSE2 (
  CFGCLK,
  DATA,
  DATAVALID
);

  `ifdef XIL_TIMING

    parameter LOC = "UNPLACED";

  `endif //

  output CFGCLK;
  output DATAVALID;
  output [31:0] DATA;

  specify

    specparam PATHPULSE$ = 0;
  endspecify
endmodule

`endcelldefine
