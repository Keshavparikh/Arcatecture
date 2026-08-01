package lightningrv.csr

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Fully Associative Translation Lookaside Buffer (TLB)
  */
class TLBApexEntry extends Bundle {
  val valid = Bool()
  val vpn   = UInt(27.W)
  val ppn   = UInt(44.W)
}

class TLBApex(numEntries: Int = 32)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqVpn   = Input(UInt(27.W))
    val reqValid = Input(Bool())
    val sfenceVma= Input(Bool())

    val hit      = Output(Bool())
    val respPpn  = Output(UInt(44.W))

    val refillEn = Input(Bool())
    val refillVpn= Input(UInt(27.W))
    val refillPpn= Input(UInt(44.W))
  })

  val entries   = RegInit(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new TLBApexEntry))))
  val victimIdx = RegInit(0.U(log2Up(numEntries).W))

  val hits = Wire(Vec(numEntries, Bool()))
  for (i <- 0 until numEntries) {
    hits(i) := entries(i).valid && (entries(i).vpn === io.reqVpn)
  }

  val hitIdx = PriorityEncoder(hits)
  io.hit     := io.reqValid && hits.asUInt.orR
  io.respPpn := entries(hitIdx).ppn

  when(io.refillEn) {
    entries(victimIdx).valid := true.B
    entries(victimIdx).vpn   := io.refillVpn
    entries(victimIdx).ppn   := io.refillPpn
    victimIdx := victimIdx + 1.U
  }

  when(io.sfenceVma) {
    for (i <- 0 until numEntries) { entries(i).valid := false.B }
  }
}
