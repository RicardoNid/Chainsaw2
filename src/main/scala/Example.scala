package org.datenlord

object Example extends App {

  TimeSpaceFlow(16, 4, 2, 8).generateWaveform(s"bubbley", "y")
  CyclicFlow(1, 16).generateWaveform(s"serialy", "y")

}
