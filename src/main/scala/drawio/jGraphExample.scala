package org.datenlord
package drawio


import java.io.File
import com.mxgraph.layout.mxOrganicLayout
import com.mxgraph.layout.mxIGraphLayout
import com.mxgraph.util.mxCellRenderer
import org.jgrapht.ext.JGraphXAdapter
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import java.awt.Color
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.IOException

object jGraphExample {
  def main(args: Array[String]): Unit = {

    val g = new DefaultDirectedGraph[String, DefaultEdge](classOf[DefaultEdge])

    val x1 = "x1"
    val x2 = "x2"
    val x3 = "x3"

    g.addVertex(x1)
    g.addVertex(x2)
    g.addVertex(x3)

    g.addEdge(x1, x2)
    g.addEdge(x2, x3)

    val graphAdapter = new JGraphXAdapter[String, DefaultEdge](g)
    val layout = new mxOrganicLayout(graphAdapter)
    layout.execute(graphAdapter.getDefaultParent)

    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val imgFile = new File("./test.png")
    ImageIO.write(image, "PNG", imgFile)
  }
}