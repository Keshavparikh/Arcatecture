package lightningrv

import chisel3._
import chisel3.util._

/**
  * Translation Lookaside Buffer (TLB)
  * 
  * Specifications:
  * - Size: Configurable (32-entry I-TLB / 64-entry D-TLB).
  * - Mode: Fully Associative single-cycle virtual to physical translation for Sv39.
  * - Invalidation: `SFENCE.VMA` flushes all valid entries.
  */

class TLBEntry extends Bundle {
  val valid = Bool()
  val vpn   = UInt(27.W) // 27-bit Virtual Page Number for Sv39 (39-bit VA: VPN[2]=9, VPN[1]=9, VPN[0]=9)
  val ppn   = UInt(44.W) // 44-bit Physical Page Number (56-bit PA)
  val r     = Bool()
  val w     = Bool()
  val x     = Bool()
  val u     = Bool()
}

class TLB(numEntries: Int = 32) extends Module {
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
    val refillR   = Input(Bool())
    val refillW   = Input(Bool())
    val refillX   = Input(Bool())
    val refillU   = Input(Bool())
  })

  val entries = RegInit(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new TLBEntry))))
  val victimIdx = RegInit(0.U(log2Up(numEntries).W))

  // Entry Lookup across all fully associative slots
  val hits = Wire(Vec(numEntries, Bool()))
  for (i <- 0 until numEntries) {
    hits(i) := entries(i).valid && (entries(i).vpn === io.reqVpn)
  }

  val hitIndex = PriorityEncoder(hits)
  io.hit     := io.reqValid && hits.asUInt.orR
  io.respPpn := entries(hitIndex).ppn

  // Refill Logic
  when(io.refillEn) {
    entries(victimIdx).valid := true.B
    entries(victimIdx).vpn   := io.refillVpn
    entries(victimIdx).ppn   := io.refillPpn
    entries(victimIdx).r     := io.refillR
    entries(victimIdx).w     := io.refillW
    entries(victimIdx).x     := io.refillX
    entries(victimIdx).u     := io.refillU

    victimIdx := victimIdx + 1.U
  }

  // SFENCE.VMA Flush Logic
  when(io.sfenceVma) {
    for (i <- 0 until numEntries) {
      entries(i).valid := false.B
    }
  }
}
