load_package flow
project_new LvdsWithPcie -revision LvdsWithPcie -overwrite
# 器件设定
set_global_assignment -name FAMILY "Cyclone V"
set_global_assignment -name DEVICE 5CGXFC9D6F27I7
# 设计文件设定
# top-level名称
set_global_assignment -name TOP_LEVEL_ENTITY LvdsWithPcie
# 设计源文件
# RTL文件
set_global_assignment -name VERILOG_FILE LvdsDebug.v
# IP文件
set_global_assignment -name QIP_FILE LVDSPLL.qip
set_global_assignment -name QIP_FILE LVDS14.qip
# generate_ip_file LVDS14.qip
set_global_assignment -name QIP_FILE PllP2S.qip
set_global_assignment -name QIP_FILE AsyncFifo.qip
# 时序约束
set_global_assignment -name SDC_FILE ../../code/board.sdc
# 输出路径设定
set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_files
# signaltap文件设定
set_global_assignment -name ENABLE_SIGNALTAP ON
set_global_assignment -name USE_SIGNALTAP_FILE LvdsWithPcie.stp
set_global_assignment -name SIGNALTAP_FILE LvdsDebug.stp
# 管脚分配
# PCIe
# 下面的PCIe相关引脚在xillybus中使用
set_location_assignment PIN_U22 -to pcie_perstn
set_instance_assignment -name IO_STANDARD "2.5 V" -to pcie_perstn
set_location_assignment PIN_V6 -to pcie_refclk
set_instance_assignment -name IO_STANDARD HCSL -to pcie_refclk
set_location_assignment PIN_W6 -to "pcie_refclk(n)"
set_instance_assignment -name IO_STANDARD HCSL -to "pcie_refclk(n)"
set_location_assignment PIN_V2 -to pcie_rx[3]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_rx[3]
set_location_assignment PIN_Y2 -to pcie_rx[2]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_rx[2]
set_location_assignment PIN_AB2 -to pcie_rx[1]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_rx[1]
set_location_assignment PIN_AD2 -to pcie_rx[0]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_rx[0]
set_location_assignment PIN_V1 -to "pcie_rx[3](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_rx[3](n)"
set_location_assignment PIN_Y1 -to "pcie_rx[2](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_rx[2](n)"
set_location_assignment PIN_AB1 -to "pcie_rx[1](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_rx[1](n)"
set_location_assignment PIN_AD1 -to "pcie_rx[0](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_rx[0](n)"
set_location_assignment PIN_W4 -to pcie_tx[3]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_tx[3]
set_location_assignment PIN_AA4 -to pcie_tx[2]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_tx[2]
set_location_assignment PIN_AC4 -to pcie_tx[1]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_tx[1]
set_location_assignment PIN_AE4 -to pcie_tx[0]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_tx[0]
set_location_assignment PIN_W3 -to "pcie_tx[3](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_tx[3](n)"
set_location_assignment PIN_AA3 -to "pcie_tx[2](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_tx[2](n)"
set_location_assignment PIN_AC3 -to "pcie_tx[1](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_tx[1](n)"
set_location_assignment PIN_AE3 -to "pcie_tx[0](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_tx[0](n)"
# 下面的PCIe相关引脚在xillybus中没有使用
set_location_assignment PIN_W20 -to PCIE_SMBCLK
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to PCIE_SMBCLK
set_location_assignment PIN_W21 -to PCIE_SMBDAT
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to PCIE_SMBDAT
set_location_assignment PIN_Y23 -to PCIE_WAKE_n
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to PCIE_WAKE_n
set_location_assignment PIN_W8 -to Pulse_Single
# xillydemo中的user_led,没有实际对应
set_location_assignment PIN_Y26 -to user_led[0]
set_instance_assignment -name IO_STANDARD "1.5 V" -to user_led[0]
set_location_assignment PIN_Y25 -to user_led[1]
set_instance_assignment -name IO_STANDARD "1.5 V" -to user_led[1]
set_location_assignment PIN_F26 -to user_led[2]
set_instance_assignment -name IO_STANDARD "1.5 V" -to user_led[2]
set_location_assignment PIN_G26 -to user_led[3]
set_instance_assignment -name IO_STANDARD "1.5 V" -to user_led[3]
# 全局异步active-low复位
set_location_assignment PIN_Y24 -to rstn
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to rstn
# ADC
set_location_assignment PIN_K25 -to adc_clk
set_instance_assignment -name IO_STANDARD LVDS -to adc_clk
set_location_assignment PIN_K26 -to "adc_clk(n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_clk(n)"
set_location_assignment PIN_T23 -to adc_in_a[6]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_a[6]
set_location_assignment PIN_T26 -to adc_in_a[5]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_a[5]
set_location_assignment PIN_R24 -to adc_in_a[4]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_a[4]
set_location_assignment PIN_P21 -to adc_in_a[3]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_a[3]
set_location_assignment PIN_N25 -to adc_in_a[2]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_a[2]
set_location_assignment PIN_R23 -to adc_in_a[1]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_a[1]
set_location_assignment PIN_M25 -to adc_in_a[0]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_a[0]
set_location_assignment PIN_T24 -to "adc_in_a[6](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_a[6](n)"
set_location_assignment PIN_R26 -to "adc_in_a[5](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_a[5](n)"
set_location_assignment PIN_R25 -to "adc_in_a[4](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_a[4](n)"
set_location_assignment PIN_P22 -to "adc_in_a[3](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_a[3](n)"
set_location_assignment PIN_P26 -to "adc_in_a[2](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_a[2](n)"
set_location_assignment PIN_P23 -to "adc_in_a[1](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_a[1](n)"
set_location_assignment PIN_M26 -to "adc_in_a[0](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_a[0](n)"
set_location_assignment PIN_N24 -to adc_in_b[6]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_b[6]
set_location_assignment PIN_K24 -to adc_in_b[5]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_b[5]
set_location_assignment PIN_N23 -to adc_in_b[4]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_b[4]
set_location_assignment PIN_H22 -to adc_in_b[3]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_b[3]
set_location_assignment PIN_L22 -to adc_in_b[2]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_b[2]
set_location_assignment PIN_J20 -to adc_in_b[1]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_b[1]
set_location_assignment PIN_H19 -to adc_in_b[0]
set_instance_assignment -name IO_STANDARD LVDS -to adc_in_b[0]
set_location_assignment PIN_M24 -to "adc_in_b[6](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_b[6](n)"
set_location_assignment PIN_K23 -to "adc_in_b[5](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_b[5](n)"
set_location_assignment PIN_M22 -to "adc_in_b[4](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_b[4](n)"
set_location_assignment PIN_J23 -to "adc_in_b[3](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_b[3](n)"
set_location_assignment PIN_K21 -to "adc_in_b[2](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_b[2](n)"
set_location_assignment PIN_J21 -to "adc_in_b[1](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_b[1](n)"
set_location_assignment PIN_H20 -to "adc_in_b[0](n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_in_b[0](n)"

set_location_assignment PIN_R20 -to BUF_CLK
set_instance_assignment -name IO_STANDARD LVDS -to BUF_CLK
set_location_assignment PIN_T21 -to DDS_clk
set_instance_assignment -name IO_STANDARD LVDS -to DDS_clk
set_location_assignment PIN_T22 -to "DDS_clk(n)"
set_instance_assignment -name IO_STANDARD LVDS -to "DDS_clk(n)"
set_location_assignment PIN_P20 -to "BUF_CLK(n)"
set_instance_assignment -name IO_STANDARD LVDS -to "BUF_CLK(n)"
# DDR3
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[14]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[13]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[12]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[11]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[10]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[9]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[8]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[7]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[6]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[5]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[4]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[3]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[2]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[1]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[14]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[13]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[12]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[11]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[10]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[9]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[8]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[7]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[6]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[5]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[4]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[3]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[2]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[1]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_A_PONG[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_BA_PING[2]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_BA_PING[1]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_BA_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_BA_PONG[2]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_BA_PONG[1]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_BA_PONG[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_CASn_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_CASn_PONG[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_CKE_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_CKE_PONG[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_CK_PING[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_CK_PING[0](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_CK_PONG[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_CK_PONG[0](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_CKn_PING[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_CKn_PING[0](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_CKn_PONG[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_CKn_PONG[0](n)"
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_CSn_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_CSn_PONG[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DM_PING[1]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DM_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DM_PONG[1]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DM_PONG[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_DQS_PING[1]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_DQS_PING[1](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_DQS_PING[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_DQS_PING[0](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_DQS_PONG[1]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_DQS_PONG[1](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_DQS_PONG[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_DQS_PONG[0](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_DQSn_PING[1]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_DQSn_PING[1](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_DQSn_PING[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_DQSn_PING[0](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_DQSn_PONG[1]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_DQSn_PONG[1](n)"
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to DDR3_DQSn_PONG[0]
set_instance_assignment -name IO_STANDARD "DIFFERENTIAL 1.35-V SSTL" -to "DDR3_DQSn_PONG[0](n)"
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[15]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[14]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[13]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[12]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[11]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[10]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[9]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[8]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[7]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[6]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[5]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[4]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[3]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[2]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[1]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[15]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[14]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[13]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[12]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[11]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[10]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[9]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[8]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[7]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[6]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[5]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[4]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[3]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[2]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[1]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_DQ_PONG[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_ODT_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_ODT_PONG[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_RASn_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_RASn_PONG[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_RESETn_PING
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_RESETn_PONG
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_RZQIN_PING
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_RZQIN_PONG
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_WEn_PING[0]
set_instance_assignment -name IO_STANDARD "SSTL-135" -to DDR3_WEn_PONG[0]
# DDS
set_location_assignment PIN_T7 -to DDSP[3]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDSP[3]
set_location_assignment PIN_AC23 -to DDSP[2]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDSP[2]
set_location_assignment PIN_V8 -to DDSP[1]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDSP[1]
set_location_assignment PIN_R8 -to DDSP[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDSP[0]
set_location_assignment PIN_U19 -to DDS_CS
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_CS
set_location_assignment PIN_AB24 -to DDS_IOUPDATE
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_IOUPDATE
set_location_assignment PIN_R9 -to DDS_RST
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_RST
set_location_assignment PIN_P8 -to DDS_SCLK
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_SCLK
set_location_assignment PIN_AC24 -to DDS_SDIO[3]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_SDIO[3]
set_location_assignment PIN_AA23 -to DDS_SDIO[2]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_SDIO[2]
set_location_assignment PIN_AA22 -to DDS_SDIO[1]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_SDIO[1]
set_location_assignment PIN_V20 -to DDS_SDIO[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_SDIO[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to "DDS_SDIO[3](n)"
set_location_assignment PIN_AD6 -to DDS_SYNC_CLK
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to DDS_SYNC_CLK

set_location_assignment PIN_AC25 -to EMMC_CLK
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_CLK
set_location_assignment PIN_AD25 -to EMMC_CMD
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_CMD
set_location_assignment PIN_AB25 -to EMMC_DATA[7]
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DATA[7]
set_location_assignment PIN_AA24 -to EMMC_DATA[6]
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DATA[6]
set_location_assignment PIN_W26 -to EMMC_DATA[5]
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DATA[5]
set_location_assignment PIN_W25 -to EMMC_DATA[4]
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DATA[4]
set_location_assignment PIN_V25 -to EMMC_DATA[3]
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DATA[3]
set_location_assignment PIN_U24 -to EMMC_DATA[2]
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DATA[2]
set_location_assignment PIN_U25 -to EMMC_DATA[1]
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DATA[1]
set_location_assignment PIN_U26 -to EMMC_DATA[0]
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DATA[0]
set_location_assignment PIN_AB26 -to EMMC_DS
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_DS
set_location_assignment PIN_AA26 -to EMMC_RST_N
set_instance_assignment -name IO_STANDARD "2.5 V" -to EMMC_RST_N
# 可调放大器
set_location_assignment PIN_R10 -to vga_b[5]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to vga_b[5]
set_location_assignment PIN_Y9 -to vga_b[4]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to vga_b[4]
set_location_assignment PIN_Y8 -to vga_b[3]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to vga_b[3]
set_location_assignment PIN_AA7 -to vga_b[2]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to vga_b[2]
set_location_assignment PIN_AB6 -to vga_b[1]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to vga_b[1]
set_location_assignment PIN_AA6 -to vga_b[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to vga_b[0]
# 端口5,6,7输出
set_location_assignment PIN_U7 -to debug0
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to debug0
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to Pulse_Single
set_location_assignment PIN_F26 -to Pulseout0
set_instance_assignment -name IO_STANDARD "2.5 V" -to Pulseout0
set_location_assignment PIN_G26 -to Pulseout0N
set_instance_assignment -name IO_STANDARD "2.5 V" -to Pulseout0N
set_location_assignment PIN_Y25 -to Pulseout1
set_instance_assignment -name IO_STANDARD "2.5 V" -to Pulseout1
set_location_assignment PIN_Y26 -to Pulseout1N
set_instance_assignment -name IO_STANDARD "2.5 V" -to Pulseout1N

set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to Exteral_Port[3]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to Exteral_Port[2]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to Exteral_Port[1]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to Exteral_Port[0]