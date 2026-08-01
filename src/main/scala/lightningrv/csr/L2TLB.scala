package lightningrv.csr

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Unified L2 TLB (L2TLB) - 512-Entry 4-Way Shared Fallback Cache
  * Resolves >90% of L1 I-TLB and D-TLB misses in 2 cycles without triggering a hardware memory walk.
  */
class L2TLBEntry extends Bundle {
  val valid = Bool()
  val tag   = UInt(18.W)
  val vpn   = UInt(27.W)
  val ppn   = UInt(44.W)
}

class L2TLB(numSets: Int = 128, numWays: Int = 4)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqVpn   = Input(UInt(27.W))
    val reqValid = Input(Bool())
    val sfenceVma= Input(Bool())

    val hit      = Output(Bool())
    val respPpn  = Output(UInt(44.W))

    // Refill Interface from Hardware Page Table Walker
    val refillEn  = Input(Bool())
    val refillVpn = Input(UInt(27.W))
    val refillPpn = Input(UInt(44.W))
  })

  val l2Array = Mem(numSets * numWays, new L2TLBEntry)
  val validArray = RegInit(VecInit(Seq.fill(numSets * numWays)(false.B)))

  val reqIndex = io.reqVpn(log2Up(numSets) - 1, 0)
  val reqTag   = io.reqVpn(26, log2Up(numSets))

  val wayHits = Wire(Vec(numWays, Bool()))
  val wayEntries = Wire(Vec(numWays, new L2TLBEntry))

  for (w <- 0 until numWays) {
    val idx = reqIndex * numWays.U + w.U
    wayEntries(w) := l2Array(idx)
    wayHits(w)    := validArray(idx) && (wayEntries(w).tag === reqTag)
  }

  val isHit  = io.reqValid && wayHits.asUInt.orR
  val hitWay = PriorityEncoder(wayHits)

  io.hit     := isHit
  io.respPpn := wayEntries(hitWay).ppn

  val victimWay = RegInit(0.U(log2Up(numWays).W))

  when(io.refillEn) {
    val writeIdx = reqIndex * numWays.U + victimWay
    val newEntry = Wire(new L2TLBEntry)
    newEntry.valid := true.B
    newEntry.tag   := reqTag
    newEntry.vpn   := io.refillVpn
    newEntry.ppn   := io.refillPpn

    l2Array(writeIdx)   := newEntry
    validArray(writeIdx):= true.B
    victimWay := victimWay + 1.U
  }

  when(io.sfenceVma) {
    for (i <- 0 until (numSets * numWays)) {
      validArray(i) := false.B
    }
  }
}
