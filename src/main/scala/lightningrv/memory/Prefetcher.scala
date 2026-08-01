package lightningrv.memory

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Hardware Stride Memory Prefetcher (Prefetcher)
  * Detects constant address strides on vector/scalar memory loads and issues prefetch requests.
  */
class Prefetcher(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val loadValid    = Input(Bool())
    val loadAddr     = Input(UInt(64.W))

    val prefetchReq   = Output(Bool())
    val prefetchAddr  = Output(UInt(64.W))
  })

  val lastAddr   = RegInit(0.U(64.W))
  val lastStride = RegInit(0.S(64.W))

  val currentStride = (io.loadAddr.asSInt - lastAddr.asSInt)
  val strideMatch   = (currentStride === lastStride) && (currentStride =/= 0.S)

  when(io.loadValid) {
    lastAddr   := io.loadAddr
    lastStride := currentStride
  }

  io.prefetchReq  := io.loadValid && strideMatch
  io.prefetchAddr := (io.loadAddr.asSInt + currentStride).asUInt
}
