package lightningrv.memory

import chisel3._
import chisel3.util._
import lightningrv._
import lightningrv.common._

/**
  * Memory Crossbar Interconnect (MemoryCrossbar)
  * Multicore-ready crossbar interconnect routing L1 I-Cache, L1 D-Cache, and DMA transactions.
  */
class MemoryCrossbar(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val master0 = new AXI4Slave(dataWidth = 64, addrWidth = 64)
    val master1 = new AXI4Slave(dataWidth = 64, addrWidth = 64)

    val slave0  = new AXI4Master(dataWidth = 64, addrWidth = 64)
  })

  val master1HasReq = io.master1.ar.valid || io.master1.aw.valid

  io.slave0.ar.valid := Mux(master1HasReq, io.master1.ar.valid, io.master0.ar.valid)
  io.slave0.ar.bits  := Mux(master1HasReq, io.master1.ar.bits, io.master0.ar.bits)

  io.slave0.aw.valid := io.master1.aw.valid
  io.slave0.aw.bits  := io.master1.aw.bits

  io.slave0.w.valid  := io.master1.w.valid
  io.slave0.w.bits   := io.master1.w.bits

  io.slave0.b.ready  := io.master1.b.ready
  io.slave0.r.ready  := Mux(master1HasReq, io.master1.r.ready, io.master0.r.ready)

  io.master0.ar.ready := io.slave0.ar.ready && !master1HasReq
  io.master0.r.valid  := io.slave0.r.valid && !master1HasReq
  io.master0.r.bits   := io.slave0.r.bits
  io.master0.aw.ready := false.B
  io.master0.w.ready  := false.B
  io.master0.b.valid  := false.B
  io.master0.b.bits   := DontCare

  io.master1.ar.ready := io.slave0.ar.ready && master1HasReq
  io.master1.r.valid  := io.slave0.r.valid && master1HasReq
  io.master1.r.bits   := io.slave0.r.bits
  io.master1.aw.ready := io.slave0.aw.ready
  io.master1.w.ready  := io.slave0.w.ready
  io.master1.b.valid  := io.slave0.b.valid
  io.master1.b.bits   := io.slave0.b.bits
}
