`timescale 1ns/1ps

module TernaryAdderXilinx
# (
  parameter width = 8
)
(
  input      [width - 1:0]    dataIn_0,
  input      [width - 1:0]    dataIn_1,
  input      [width - 1:0]    dataIn_2,
  output     [width + 1:0]    dataOut
);

  assign dataOut = dataIn_0 + dataIn_1 - dataIn_2;

endmodule
