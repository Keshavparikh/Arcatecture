package lightningrv.lsu

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Vector Memory Unit (VectorMemUnit) - Scatter/Gather Address Coalescing Engine
  * Groups non-unit stride and scatter/gather vector element addresses into 64-byte cache line buckets.
  */
class VectorMemUnit(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqValid     = Input(Bool())
    val baseAddr     = Input(UInt(64.W))
    val stride       = Input(UInt(64.W))
    val indexVector  = Input(UInt(256.W))
    val isIndexed    = Input(Bool())
    val isStore      = Input(Bool())

    val coalescedLineReq  = Output(Bool())
    val coalescedLineAddr = Output(UInt(64.W))
    val numLinesNeeded    = Output(UInt(4.W))

    val vectorDataOut     = Output(UInt(256.W))
    val cacheLineDataIn   = Input(UInt(512.W))
  })

  // Unpack 8 element indices
  val indices = Wire(Vec(8, UInt(32.W)))
  for (i <- 0 until 8) {
    indices(i) := io.indexVector(i * 32 + 31, i * 32)
  }

  // Generate 8 element physical addresses
  val elementAddrs = Wire(Vec(8, UInt(64.W)))
  for (i <- 0 until 8) {
    val offset = Mux(io.isIndexed, indices(i), (i.U * io.stride))
    elementAddrs(i) := io.baseAddr + offset
  }

  // Extract cache line tags (64-byte granularity: addr(63, 6))
  val lineTags = Wire(Vec(8, UInt(58.W)))
  for (i <- 0 until 8) {
    lineTags(i) := elementAddrs(i)(63, 6)
  }

  // Count unique cache lines needed
  val uniqueLines = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    val isFirst = if (i == 0) true.B else !lineTags.take(i).map(_ === lineTags(i)).reduce(_ || _)
    uniqueLines(i) := isFirst
  }

  io.coalescedLineReq  := io.reqValid
  io.coalescedLineAddr := Cat(lineTags(0), 0.U(6.W))
  io.numLinesNeeded    := PopCount(uniqueLines)

  // Shuffle network: pack elements from 512-bit cache line into 256-bit vector data
  val shuffledElements = Wire(Vec(8, UInt(32.W)))
  for (i <- 0 until 8) {
    val byteOffset = elementAddrs(i)(5, 0)
    shuffledElements(i) := (io.cacheLineDataIn >> (byteOffset << 3))(31, 0)
  }

  io.vectorDataOut := Cat(shuffledElements.reverse)
}
