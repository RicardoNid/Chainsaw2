package pippenger

import scala.collection.mutable._

object pippengerSim {
  def doPippenger(K: Array[BigInt], P: Array[BigInt], W: Int, w: Int, pWidth: Int, latency: Int, fifoDepth: Int, add: (BigInt, BigInt) => BigInt): Unit = {
    require(K.length == P.length)
    require(K.forall(_.bitLength <= W))
    require(w > 1)

    val N = K.length
    val G = Math.ceil(W.toDouble / w).toInt

    require(latency >= 2 * G + 1)

    print(s"N的值为${N}、W的值为${W}、w的值为${w}、G的值为${G}。\n")
    print(s"生成两个位宽为${pWidth * 3}bit、大小为${(G + 1) / 2 << w}的mem。\n")
    val mem1 = Array.fill((G + 1) / 2 << w)(BigInt(0))
    val mem1State = Array.fill((G + 1) / 2 << w)(false)
    val mem2 = Array.fill((G + 1) / 2 << w)(BigInt(0))
    val mem2State = Array.fill((G + 1) / 2 << w)(false)

    print(s"生成两个延迟为${latency}的加法器。\n")
    case class pipelineType(data: BigInt, address: Int, valid: Boolean)
    val adderPipeline1 = Queue.fill(latency)(pipelineType(0, 0, false))
    val adderPipeline2 = Queue.fill(latency)(pipelineType(0, 0, false))

    case class fifoType(data1: BigInt, data2: BigInt, address: Int)
    print(s"生成两个深度为${fifoDepth}的fifo。\n")
    val fifo1 = new Queue[fifoType]
    val fifo2 = new Queue[fifoType]

    var period = Array.fill(3)(BigInt(0))
    var workPeriod1 = Array.fill(3)(BigInt(0))
    var workPeriod2 = Array.fill(3)(BigInt(0))
    var fifo1MaxUsage = 0
    var fifo2MaxUsage = 0

    print("\n开始第一步计算。\n")

    var i = 0
    var j = 0
    var blocked = false
    while (i < N) {
      j = 0
      while (j < G) {
        period(0) += 1
        blocked = false

        //用fifo更新
        if (fifo1.nonEmpty) {
          val fifoOutput = fifo1.dequeue()
          adderPipeline1.enqueue(pipelineType(add(fifoOutput.data1, fifoOutput.data2), fifoOutput.address, true))
          workPeriod1(0) += 1
        } else {
          adderPipeline1.enqueue(pipelineType(0, 0, false))
        }
        if (fifo2.nonEmpty) {
          val fifoOutput = fifo2.dequeue()
          adderPipeline2.enqueue(pipelineType(add(fifoOutput.data1, fifoOutput.data2), fifoOutput.address, true))
          workPeriod2(0) += 1
        } else {
          adderPipeline2.enqueue(pipelineType(0, 0, false))
        }

        val b1 = ((K(i) >> (w * j)) & ((1 << w) - 1)).toInt
        val address1 = (j << (w - 1)) | b1
        val adder1Output = adderPipeline1.dequeue()
        if (adder1Output.valid && adder1Output.address == address1) {
          if (b1 != 0) {
            fifo1.enqueue(fifoType(P(i), adder1Output.data, address1))
          }
        } else {
          if (adder1Output.valid) {
            if (mem1State(adder1Output.address)) {
              fifo1.enqueue(fifoType(adder1Output.data, mem1(adder1Output.address), adder1Output.address))
              mem1State(adder1Output.address) = false
            } else {
              mem1(adder1Output.address) = adder1Output.data
              mem1State(adder1Output.address) = true
            }
          }

          if (b1 != 0 && mem1State(address1) && fifo1.size == fifoDepth) {
            blocked = true
          }
        }

        val b2 = ((K(i) >> (w * (j + 1))) & ((1 << w) - 1)).toInt
        val address2 = (j << (w - 1)) | b2
        val adder2Output = adderPipeline2.dequeue()
        if (adder2Output.valid && adder2Output.address == address2) {
          if (b2 != 0) {
            fifo2.enqueue(fifoType(P(i), adder2Output.data, address2))
          }
        } else {
          if (adder2Output.valid) {
            if (mem2State(adder2Output.address)) {
              fifo2.enqueue(fifoType(adder2Output.data, mem2(adder2Output.address), adder2Output.address))
              mem2State(adder2Output.address) = false
            } else {
              mem2(adder2Output.address) = adder2Output.data
              mem2State(adder2Output.address) = true
            }
          }

          if (b2 != 0 && mem2State(address2) && fifo2.size == fifoDepth) {
            blocked = true
          }
        }


        if (!blocked) {
          if (!(adder1Output.valid && adder1Output.address == address1)) {
            if (b1 != 0) {
              if (mem1State(address1)) {
                fifo1.enqueue(fifoType(P(i), mem1(address1), address1))
                mem1State(address1) = false
              } else {
                mem1(address1) = P(i)
                mem1State(address1) = true
              }
            }
          }

          if (!(adder2Output.valid && adder2Output.address == address2)) {
            if (b2 != 0) {
              if (mem2State(address2)) {
                fifo2.enqueue(fifoType(P(i), mem2(address2), address2))
                mem2State(address2) = false
              } else {
                mem2(address2) = P(i)
                mem2State(address2) = true
              }
            }
          }

          j += 2
        }

        if (fifo1.size > fifo1MaxUsage) {
          fifo1MaxUsage = fifo1.size
        }
        if (fifo2.size > fifo2MaxUsage) {
          fifo2MaxUsage = fifo2.size
        }
      }
      i += 1
    }

    //等待全部计算完成
    var over1 = false
    var fifo1Empty = false
    var waitCnt1 = 0
    var over2 = false
    var fifo2Empty = false
    var waitCnt2 = 0
    while (!over1 || !over2) {
      period(0) += 1
      if (!over1) {
        if (fifo1Empty) {
          if (fifo1.nonEmpty) {
            fifo1Empty = false
            waitCnt1 = 0
          } else {
            waitCnt1 += 1
            if (waitCnt1 == latency) {
              over1 = true
            }
          }
        } else {
          if (fifo1.isEmpty) {
            fifo1Empty = true
          }
        }
      }

      if (!over2) {
        if (fifo2Empty) {
          if (fifo2.nonEmpty) {
            fifo2Empty = false
            waitCnt2 = 0
          } else {
            waitCnt2 += 1
            if (waitCnt2 == latency) {
              over2 = true
            }
          }
        } else {
          if (fifo2.isEmpty) {
            fifo2Empty = true
          }
        }
      }

      if (fifo1.nonEmpty) {
        val fifoOutput = fifo1.dequeue()
        adderPipeline1.enqueue(pipelineType(add(fifoOutput.data1, fifoOutput.data2), fifoOutput.address, true))
        workPeriod1(0) += 1
      } else {
        adderPipeline1.enqueue(pipelineType(0, 0, false))
      }

      val adder1Output = adderPipeline1.dequeue()
      if (adder1Output.valid) {
        if (mem1State(adder1Output.address)) {
          fifo1.enqueue(fifoType(adder1Output.data, mem1(adder1Output.address), adder1Output.address))
          mem1State(adder1Output.address) = false
        } else {
          mem1(adder1Output.address) = adder1Output.data
          mem1State(adder1Output.address) = true
        }
      }

      if (fifo2.nonEmpty) {
        val fifoOutput = fifo2.dequeue()
        adderPipeline2.enqueue(pipelineType(add(fifoOutput.data1, fifoOutput.data2), fifoOutput.address, true))
        workPeriod2(0) += 2
      } else {
        adderPipeline2.enqueue(pipelineType(0, 0, false))
      }

      val adder2Output = adderPipeline2.dequeue()
      if (adder2Output.valid) {
        if (mem2State(adder2Output.address)) {
          fifo2.enqueue(fifoType(adder2Output.data, mem2(adder2Output.address), adder2Output.address))
          mem2State(adder2Output.address) = false
        } else {
          mem2(adder2Output.address) = adder2Output.data
          mem2State(adder2Output.address) = true
        }
      }
    }

    printf(s"第一步计算结束，共花费${period(0)}个时钟周期。\n")
    printf(s"其中加法器在做有效工作的周期分别为${workPeriod1(0)}和${workPeriod2(0)}个，占用率分别为${workPeriod1(0).toDouble / period(0).toDouble}和${workPeriod2(0).toDouble / period(0).toDouble}。\n")
    printf(s"两个fifo的最大占用深度分别为${fifo1MaxUsage}和${fifo2MaxUsage}。\n")

    print("\n开始第二步计算。\n")

    //将两个mem合为一体
    val mem = new Array[BigInt](G << w)
    for (i <- 0 until G) {
      if (i % 2 == 0) {
        for (j <- 0 until (1 << w)) {
          mem((i << w) + j) = mem1((i << (w - 1)) + j)
        }
      } else {
        for (j <- 0 until (1 << w)) {
          mem((i << w) + j) = mem2(((i & -2) << (w - 1)) + j)
        }
      }
    }

    for (i <- 0 until G) {
      period(1) += 1

      adderPipeline1.enqueue(pipelineType(mem((i << w) | ((1 << w) - 1)), 0, true))
      //这个地方相当于加0，不计入加法器的工作周期
      adderPipeline1.dequeue()

      period(1) += 1
      workPeriod1(1) += 1

      adderPipeline1.enqueue(pipelineType(mem((i << w) | ((1 << w) - 1)) + mem((i << w) | ((1 << w) - 2)), 0, true))
      adderPipeline1.dequeue()
    }

    for (i <- 0 until latency - 2 * G) {
      period(1) += 1

      adderPipeline1.enqueue(pipelineType(0, 0, false))
      adderPipeline1.dequeue()
    }

    for (i <- (1 until (1 << w) - 2).reverse) {
      for (j <- 0 until G) {
        period(1) += 2
        workPeriod1(1) += 2

        val adder1Output1 = adderPipeline1.dequeue()
        val adder1Output2 = adderPipeline1.dequeue()

        adderPipeline1.enqueue(pipelineType(adder1Output1.data + adder1Output2.data, 0, true))
        adderPipeline1.enqueue(pipelineType(adder1Output2.data + mem((j << w) | i), 0, true))
      }

      for (i <- 0 until latency - 2 * G) {
        period(1) += 1

        adderPipeline1.enqueue(pipelineType(0, 0, false))
        adderPipeline1.dequeue()
      }
    }

    for (i <- 0 until G) {
      period(1) += 2
      workPeriod1(1) += 1

      var adder1Output1 = adderPipeline1.dequeue()
      val adder1Output2 = adderPipeline1.dequeue()

      adderPipeline1.enqueue(pipelineType(adder1Output1.data + adder1Output2.data, 0, true))
      adderPipeline1.enqueue(pipelineType(0, 0, false))
    }

    for (i <- 0 until latency - 2 * G - 1) {
      period(1) += 1

      adderPipeline1.enqueue(pipelineType(0, 0, false))
      adderPipeline1.dequeue()
    }

    for (i <- 0 until G - 1) {
      period(1) += 1

      adderPipeline1.dequeue()
      adderPipeline1.enqueue(pipelineType(0, 0, false))

      period(1) += 1

      val adder1Output = adderPipeline1.dequeue()
      adderPipeline1.enqueue(pipelineType(0, 0, false))

      mem(i << w) = adder1Output.data
    }

    period(1) += 1

    adderPipeline1.dequeue()
    adderPipeline1.enqueue(pipelineType(0, 0, false))
    //将最高位的结果留在adder里，下个周期直接开始第三步

    printf(s"第二步计算结束，共花费${period(1)}个时钟周期。\n")
    printf(s"其中加法器在做有效工作的周期为${workPeriod1(1)}个，占用率为${workPeriod1(1).toDouble / period(1).toDouble}。\n")

    print("\n开始第三步计算。\n")

    for (i <- (0 until G - 1).reverse) {
      for (j <- 0 until w) {
        period(2) += 1
        workPeriod1(2) += 1

        val adder1Output = adderPipeline1.dequeue()
        adderPipeline1.enqueue(pipelineType(adder1Output.data * 2, 0, true))

        for (k <- 0 until latency - 1) {
          period(2) += 1

          adderPipeline1.dequeue()
          adderPipeline1.enqueue(pipelineType(0, 0, false))
        }
      }
      period(2) += 1
      workPeriod1(2) += 1

      val adder1Output = adderPipeline1.dequeue()
      adderPipeline1.enqueue(pipelineType(adder1Output.data + mem(i << w), 0, true))

      for (k <- 0 until latency - 1) {
        period(2) += 1

        adderPipeline1.dequeue()
        adderPipeline1.enqueue(pipelineType(0, 0, false))
      }
    }

    printf(s"第三步计算结束，共花费${period(2)}个时钟周期。\n")
    printf(s"其中加法器在做有效工作的周期为${workPeriod1(2)}个，占用率为${workPeriod1(2).toDouble / period(2).toDouble}。\n")

    printf(s"\n花费的总时钟周期为${period.sum}，其中三步占比分别为${period(0).toDouble / period.sum.toDouble}、${period(1).toDouble / period.sum.toDouble}和${period(2).toDouble / period.sum.toDouble}。\n")
    printf(s"其中加法器在做有效工作的周期分别为${workPeriod1.sum}和${workPeriod2.sum}，占用率分别为${workPeriod1.sum.toDouble / period.sum.toDouble}和${workPeriod2.sum.toDouble / period.sum.toDouble}。\n")

    printf("\n开始进行正确性验证。\n")

    val goldenSum = K.zip(P).map { case (q, p) => p * q }.sum
    val outputSum = adderPipeline1.dequeue().data

    if (outputSum == goldenSum) {
      print("结果正确。\n")
    } else {
      print("结果错误。\n")
    }
  }

  def main(args: Array[String]): Unit = {
    val N = 1024 * 1024 * 64
    val W = 253
    val w = 10
    val pWidth = 377
    val latency = 300
    val fifoDepth = 1024

    val K = Array.fill(N)(BigInt(W, scala.util.Random))
    val P = Array.fill(N)(BigInt(16, scala.util.Random)) //仿真用

    doPippenger(K, P, W, w, pWidth, latency, fifoDepth, _ + _)
  }
}