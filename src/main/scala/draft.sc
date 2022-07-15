val leadingPattern = "CLB LUTs\\*"
val targetPattern = "[0-9]\\d*"
val combined = s"$leadingPattern.*($targetPattern)"

combined.r.findFirstMatchIn("| CLB LUTs*               |  127 |     0 |          0 |   1182240 |  0.01 |").get.group(1)