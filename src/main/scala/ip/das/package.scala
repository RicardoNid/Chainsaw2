package org.datenlord
package ip

import spinal.core.{ClockDomainConfig, LOW}

package object das {

  val dasClockConfig = ClockDomainConfig(resetActiveLevel = LOW)

  val processFactor = 2 // frequency of data processing / frequency of PCIe upstream
}
