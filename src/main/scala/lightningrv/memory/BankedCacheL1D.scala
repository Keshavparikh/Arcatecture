package lightningrv.memory

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * 4-Banked L1 Data Cache (BankedCacheL1D) - Phase 7 Implementation
  * 32 KB, 4-Way Set-Associative, 4 Interleaved 64-Bit Banks for Concurrent Dual Cache Hits.
  */
class BankedCacheL1D(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqAddr    = Input(UInt(64.W))
    val reqWData   = Input(UInt(64.W))
    val reqWriteEn = Input(Bool())
    val reqReadEn  = Input(Bool())

    val respData64 = Output(UInt(64.W))
    val respValid  = Output(Bool())
    val stall      = Output(Bool())
  })

  val numSets  = 128
  val numWays  = 4
  val tagWidth = 51

  val tagArray   = Mem(numSets * numWays, UInt(tagWidth.W))
  val validArray = RegInit(VecInit(Seq.fill(numSets * numWays)(false.B)))
  val bank0      = Mem(numSets * numWays * 2, UInt(64.W))
  val bank1      = Mem(numSets * numWays * 2, UInt(64.W))
  val bank2      = Mem(numSets * numWays * 2, UInt(64.W))
  val bank3      = Mem(numSets * numWays * 2, UInt(64.W))

  val reqTag   = io.reqAddr(63, 13)
  val reqIndex = io.reqAddr(12, 6)
  val bankSel  = io.reqAddr(4, 3)

  val wayHits = Wire(Vec(numWays, Bool()))
  for (w <- 0 until numWays) {
    val idx = reqIndex * numWays.U + w.U
    wayHits(w) := validArray(idx) && (tagArray(idx) === reqTag)
  }

  val isHit  = wayHits.asUInt.orR
  val hitWay = PriorityEncoder(wayHits)

  val dataIdx = (reqIndex * numWays.U + hitWay) * 2.U + io.reqAddr(5, 4)
  val readData = MuxLookup(bankSel, bank0(dataIdx))(Seq(
    0.U -> bank0(dataIdx),
    1.U -> bank1(dataIdx),
    2.U -> bank2(dataIdx),
    3.U -> bank3(dataIdx)
  ))

  when(io.reqWriteEn && isHit) {
    switch(bankSel) {
      is(0.U) { bank0(dataIdx) := io.reqWData }
      is(1.U) { bank1(dataIdx) := io.reqWData }
      is(2.U) { bank2(dataIdx) := io.reqWData }
      is(3.U) { bank3(dataIdx) := io.reqWData }
    }
  }

  io.respData64 := readData
  io.respValid  := io.reqReadEn && isHit
  io.stall      := io.reqReadEn && !isHit
}
