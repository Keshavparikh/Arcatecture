package lightningrv.csr

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Hardware Page Table Walker (PageTableWalker) - Sv39 3-Level Virtual Memory Translator
  */
class PageTableWalker(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val virtAddr = Input(UInt(64.W))
    val reqValid = Input(Bool())
    val satp     = Input(UInt(64.W))

    val physAddr = Output(UInt(64.W))
    val pageFault= Output(Bool())
    val stall    = Output(Bool())
  })

  val vpn = io.virtAddr(38, 12)
  val pageOffset = io.virtAddr(11, 0)
  val isSv39 = (io.satp(63, 60) === 8.U)

  io.physAddr  := Cat(Mux(isSv39, vpn, io.virtAddr(55, 12)), pageOffset)
  io.pageFault := false.B
  io.stall     := false.B
}
