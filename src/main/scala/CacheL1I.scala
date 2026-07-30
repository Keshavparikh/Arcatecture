package lightningrv

import chisel3._
import chisel3.util._

/**
  * L1 Instruction Cache (CacheL1I)
  * 
  * Specifications:
  * - Size: 32 KB, 4-Way Set-Associative.
  * - Line Size: 64 Bytes (16 x 32-bit instructions per cache line).
  * - Set Count: 128 Sets (Index = addr(12, 6), Offset = addr(5, 0), Tag = addr(63, 13)).
  * - Refill: 64-bit AXI4 Read Channel with burst length = 7 (8 x 64-bit beats = 64 bytes).
  * - Invalidation: FENCE.I invalidates all valid bits across all 128 sets.
  */
class CacheL1I extends Module {
  val io = IO(new Bundle {
    // CPU Fetch Port (64-Bit Virtual Address / Dual 32-Bit Instruction Output)
    val reqAddr    = Input(UInt(64.W))
    val fetchValid = Input(Bool())
    val fenceI     = Input(Bool())

    val respData64 = Output(UInt(64.W))
    val respValid  = Output(Bool())
    val stall      = Output(Bool())

    // AXI4 Master Interface to Main Memory / System Interconnect
    val axi = new AXI4Master(dataWidth = 64, addrWidth = 64)
  })

  val numSets  = 128
  val numWays  = 4
  val lineBytes = 64
  val tagWidth = 51

  // Cache Storage Arrays
  // 128 sets x 4 ways x 51-bit Tag Array
  val tagArray = Mem(numSets * numWays, UInt(tagWidth.W))
  val validArray = RegInit(VecInit(Seq.fill(numSets * numWays)(false.B)))

  // 128 sets x 4 ways x 8 words x 64 bits Data Array
  val dataArray = Mem(numSets * numWays * 8, UInt(64.W))

  // Pseudo-LRU Replacement State per set (3 bits per set for 4 ways)
  val lruReg = RegInit(VecInit(Seq.fill(numSets)(0.U(3.W))))

  // Address Parsing
  val reqTag    = io.reqAddr(63, 13)
  val reqIndex  = io.reqAddr(12, 6)
  val reqOffset = io.reqAddr(5, 0)
  val wordIdx   = io.reqAddr(5, 3) // Which 64-bit word within 64-byte line

  // Tag Comparison across 4 ways
  val wayHits = Wire(Vec(numWays, Bool()))
  val wayTags = Wire(Vec(numWays, UInt(tagWidth.W)))

  for (w <- 0 until numWays) {
    val idx = reqIndex * numWays.U + w.U
    wayTags(w) := tagArray(idx)
    wayHits(w) := validArray(idx) && (wayTags(w) === reqTag)
  }

  val isHit  = wayHits.asUInt.orR
  val hitWay = PriorityEncoder(wayHits)

  // Data Read on Hit
  val hitDataIdx = (reqIndex * numWays.U + hitWay) * 8.U + wordIdx
  val hitData    = dataArray(hitDataIdx)

  // Refill FSM States
  val sIdle :: sAR :: sR :: sFlush :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val refillAddr   = Reg(UInt(64.W))
  val refillIndex  = refillAddr(12, 6)
  val refillTag    = refillAddr(63, 13)
  val refillBeat   = RegInit(0.U(3.W))
  val targetWay    = Reg(UInt(2.W))

  // Replacement Way Selection (Pseudo-LRU)
  val lruBits = lruReg(refillIndex)
  val victimWay = Wire(UInt(2.W))
  victimWay := Mux(!lruBits(0), Mux(!lruBits(1), 0.U, 1.U), Mux(!lruBits(2), 2.U, 3.U))

  // Default AXI Signals
  io.axi.ar.valid := false.B
  io.axi.ar.bits.addr  := Cat(refillTag, refillIndex, 0.U(6.W))
  io.axi.ar.bits.len   := 7.U // 8 beats x 8 bytes = 64 bytes
  io.axi.ar.bits.size  := 3.U // 8 bytes (64-bit)
  io.axi.ar.bits.burst := 1.U // INCR
  io.axi.ar.bits.id    := 0.U
  io.axi.ar.bits.lock  := 0.U
  io.axi.ar.bits.cache := 0.U
  io.axi.ar.bits.prot  := 0.U
  io.axi.ar.bits.qos   := 0.U
  io.axi.ar.bits.region:= 0.U

  io.axi.r.ready := false.B
  io.axi.aw.valid := false.B
  io.axi.aw.bits := DontCare
  io.axi.w.valid := false.B
  io.axi.w.bits := DontCare
  io.axi.b.ready := false.B

  switch(state) {
    is(sIdle) {
      when(io.fenceI) {
        state := sFlush
      }.elsewhen(io.fetchValid && !isHit) {
        refillAddr := io.reqAddr
        targetWay  := victimWay
        refillBeat := 0.U
        state      := sAR
      }
    }

    is(sAR) {
      io.axi.ar.valid := true.B
      when(io.axi.ar.ready) {
        state := sR
      }
    }

    is(sR) {
      io.axi.r.ready := true.B
      when(io.axi.r.valid) {
        val writeIdx = (refillIndex * numWays.U + targetWay) * 8.U + refillBeat
        dataArray(writeIdx) := io.axi.r.bits.data
        refillBeat := refillBeat + 1.U

        when(io.axi.r.bits.last || refillBeat === 7.U) {
          val tagIdx = refillIndex * numWays.U + targetWay
          tagArray(tagIdx)   := refillTag
          validArray(tagIdx) := true.B

          // Update Pseudo-LRU
          val currentLru = lruReg(refillIndex)
          val newLru     = WireDefault(currentLru)
          switch(targetWay) {
            is(0.U) { newLru := Cat(currentLru(2), 1.U(1.W), 1.U(1.W)) }
            is(1.U) { newLru := Cat(currentLru(2), 0.U(1.W), 1.U(1.W)) }
            is(2.U) { newLru := Cat(1.U(1.W), currentLru(1), 0.U(1.W)) }
            is(3.U) { newLru := Cat(0.U(1.W), currentLru(1), 0.U(1.W)) }
          }
          lruReg(refillIndex) := newLru
          state := sIdle
        }
      }
    }

    is(sFlush) {
      for (i <- 0 until (numSets * numWays)) {
        validArray(i) := false.B
      }
      state := sIdle
    }
  }

  io.stall      := (io.fetchValid && !isHit) || state =/= sIdle
  io.respValid  := io.fetchValid && isHit && (state === sIdle)
  io.respData64 := hitData
}
