`timescale 1ns/1ps

module TernaryAdderXilinxSigned
# (
  parameter width = 8,
  parameter mode = 0
)
(
  input  signed  [width - 1:0]    dataIn_0,
  input  signed  [width - 1:0]    dataIn_1,
  input  signed  [width - 1:0]    dataIn_2,
  output signed  [width + 1:0]    dataOut
);
  if(mode == 0) begin
    assign dataOut = dataIn_0 + dataIn_1 + dataIn_2;
  end
  else if(mode == 1) begin
    assign dataOut = dataIn_0 + dataIn_1 - dataIn_2;
  end
  else begin
    assign dataOut = dataIn_0 - dataIn_1 - dataIn_2;
  end

endmodule
