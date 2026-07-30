package lightningrv

import chisel3._
import chisel3.util._

/**
  * Sv39 Virtual Memory Management Unit (MMU) & Hardware Page Table Walker
  * 
  * Specifications:
  * - Translation: 39-Bit Virtual Address (`VA`) to 56-Bit Physical Address (`PA`).
  * - Page Size: 4 KB Granularity (VPN[2]=9 bits, VPN[1]=9 bits, VPN[0]=9 bits, Page Offset=12 bits).
  * - Hardware Walker: Traverses 3-level page tables in memory on TLB miss.
  */
class MMU extends Module {
  val io = IO(new Bundle {
    val virtAddr = Input(UInt(64.W))
    val reqValid = Input(Bool())
    val isExec   = Input(Bool()) // True = Instruction Fetch, False = Data Load/Store
    val satp     = Input(UInt(64.W))
    val privMode = Input(UInt(2.W))
    val sfenceVma= Input(Bool())

    val physAddr = Output(UInt(64.W))
    val pageFault= Output(Bool())
    val stall    = Output(Bool())

    // Memory Master Port for Page Table Walks
    val memReadAddr  = Output(UInt(64.W))
    val memReadValid = Output(Bool())
    val memReadData  = Input(UInt(64.W))
    val memReadReady = Input(Bool())
  })

  // TLBs
  val itlb = Module(new TLB(32))
  val dtlb = Module(new TLB(64))

  val vpn = io.virtAddr(38, 12)
  val pageOffset = io.virtAddr(11, 0)

  val isSv39 = (io.satp(63, 60) === 8.U) && (io.privMode < PrivilegeMode.Machine)

  // Direct Physical Bypass when satp mode == 0 or M-Mode
  val bypass = !isSv39

  itlb.io.reqVpn    := vpn
  itlb.io.reqValid  := io.reqValid && io.isExec && isSv39
  itlb.io.sfenceVma := io.sfenceVma

  dtlb.io.reqVpn    := vpn
  dtlb.io.reqValid  := io.reqValid && !io.isExec && isSv39
  dtlb.io.sfenceVma := io.sfenceVma

  val tlbHit = Mux(io.isExec, itlb.io.hit, dtlb.io.hit)
  val tlbPpn = Mux(io.isExec, itlb.io.respPpn, dtlb.io.respPpn)

  // Hardware Page Table Walker FSM
  val sIdle :: sL2 :: sL1 :: sL0 :: sRefill :: Nil = Enum(5)
  val state = RegInit(sIdle)

  val rootPpn  = io.satp(43, 0)
  val walkAddr = Reg(UInt(64.W))
  val walkPpn  = Reg(UInt(44.W))
  val pageFaultReg = RegInit(false.B)

  val vpn2 = vpn(26, 18)
  val vpn1 = vpn(17, 9)
  val vpn0 = vpn(8, 0)

  io.memReadAddr  := walkAddr
  io.memReadValid := (state === sL2 || state === sL1 || state === sL0)

  itlb.io.refillEn  := false.B
  itlb.io.refillVpn := vpn
  itlb.io.refillPpn := walkPpn
  itlb.io.refillR   := true.B
  itlb.io.refillW   := true.B
  itlb.io.refillX   := true.B
  itlb.io.refillU   := true.B

  dtlb.io.refillEn  := false.B
  dtlb.io.refillVpn := vpn
  dtlb.io.refillPpn := walkPpn
  dtlb.io.refillR   := true.B
  dtlb.io.refillW   := true.B
  dtlb.io.refillX   := true.B
  dtlb.io.refillU   := true.B

  switch(state) {
    is(sIdle) {
      pageFaultReg := false.B
      when(io.reqValid && isSv39 && !tlbHit) {
        walkAddr := Cat(rootPpn, vpn2, 0.U(3.W))
        state    := sL2
      }
    }

    is(sL2) {
      when(io.memReadReady) {
        val pte = io.memReadData
        val pteV = pte(0)
        val pteR = pte(1)
        val pteW = pte(2)
        val pteX = pte(3)

        when(!pteV) {
          pageFaultReg := true.B
          state        := sIdle
        }.elsewhen(!pteR && !pteW && !pteX) { // Pointer to Level 1
          walkAddr := Cat(pte(53, 10), vpn1, 0.U(3.W))
          state    := sL1
        }.otherwise { // Megapage Leaf
          walkPpn := pte(53, 10)
          state   := sRefill
        }
      }
    }

    is(sL1) {
      when(io.memReadReady) {
        val pte = io.memReadData
        val pteV = pte(0)
        val pteR = pte(1)

        when(!pteV) {
          pageFaultReg := true.B
          state        := sIdle
        }.elsewhen(!pteR) { // Pointer to Level 0
          walkAddr := Cat(pte(53, 10), vpn0, 0.U(3.W))
          state    := sL0
        }.otherwise {
          walkPpn := pte(53, 10)
          state   := sRefill
        }
      }
    }

    is(sL0) {
      when(io.memReadReady) {
        val pte = io.memReadData
        val pteV = pte(0)
        when(!pteV) {
          pageFaultReg := true.B
          state        := sIdle
        }.otherwise {
          walkPpn := pte(53, 10)
          state   := sRefill
        }
      }
    }

    is(sRefill) {
      when(io.isExec) {
        itlb.io.refillEn := true.B
      }.otherwise {
        dtlb.io.refillEn := true.B
      }
      state := sIdle
    }
  }

  val finalPpn = Mux(bypass, io.virtAddr(55, 12), Mux(tlbHit, tlbPpn, walkPpn))

  io.physAddr  := Cat(finalPpn, pageOffset)
  io.pageFault := pageFaultReg
  io.stall     := io.reqValid && isSv39 && !tlbHit && (state =/= sIdle)
}
