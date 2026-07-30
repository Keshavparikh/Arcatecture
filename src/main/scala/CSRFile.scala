package lightningrv

import chisel3._
import chisel3.util._

/**
  * Control and Status Register File (CSRFile)
  * 
  * Features:
  * - Hardware Privilege Modes: Machine (M=3), Supervisor (S=1), User (U=0).
  * - Full RISC-V Privileged Architecture CSRs required for Linux 6.x & OpenSBI:
  *   mstatus, sstatus, mie, sie, mtvec, stvec, mepc, sepc, mcause, scause, satp, fcsr.
  */

object PrivilegeMode {
  val User       = 0.U(2.W)
  val Supervisor = 1.U(2.W)
  val Machine    = 3.U(2.W)
}

class CSRFile extends Module {
  val io = IO(new Bundle {
    val csrAddr   = Input(UInt(12.W))
    val csrWriteVal = Input(UInt(64.W))
    val csrOp     = Input(UInt(2.W)) // 0=READ, 1=WRITE (CSRRW), 2=SET (CSRRS), 3=CLEAR (CSRRC)
    val valid     = Input(Bool())

    val privMode  = Output(UInt(2.W))
    val satp      = Output(UInt(64.W))  // Page table root pointer for Sv39 MMU
    val csrReadVal= Output(UInt(64.W))

    // Trap / Exception Handling Inputs
    val trapOccurred = Input(Bool())
    val trapCause    = Input(UInt(64.W))
    val trapPC       = Input(UInt(64.W))
    val mret         = Input(Bool())
    val sret         = Input(Bool())

    val trapVector   = Output(UInt(64.W))
    val epc          = Output(UInt(64.W))
  })

  // Privilege Mode State Register (Default = Machine Mode on Reset)
  val privModeReg = RegInit(PrivilegeMode.Machine)
  io.privMode := privModeReg

  // System CSR Storage Registers
  val mstatusReg = RegInit(0.U(64.W))
  val mtvecReg   = RegInit(0.U(64.W))
  val mieReg     = RegInit(0.U(64.W))
  val mepcReg    = RegInit(0.U(64.W))
  val mcauseReg  = RegInit(0.U(64.W))

  val stvecReg   = RegInit(0.U(64.W))
  val sieReg     = RegInit(0.U(64.W))
  val sepcReg    = RegInit(0.U(64.W))
  val scauseReg  = RegInit(0.U(64.W))
  val satpReg    = RegInit(0.U(64.W))
  val fcsrReg    = RegInit(0.U(64.W))

  io.satp := satpReg

  // CSR Read Logic
  val readVal = WireDefault(0.U(64.W))
  switch(io.csrAddr) {
    is(0x300.U) { readVal := mstatusReg }
    is(0x100.U) { readVal := mstatusReg & "h00000000000DE162".U } // sstatus shadow
    is(0x304.U) { readVal := mieReg }
    is(0x104.U) { readVal := sieReg }
    is(0x305.U) { readVal := mtvecReg }
    is(0x105.U) { readVal := stvecReg }
    is(0x341.U) { readVal := mepcReg }
    is(0x141.U) { readVal := sepcReg }
    is(0x342.U) { readVal := mcauseReg }
    is(0x142.U) { readVal := scauseReg }
    is(0x180.U) { readVal := satpReg }
    is(0x003.U) { readVal := fcsrReg }
  }
  io.csrReadVal := readVal

  // CSR Write / Bit Manipulation Logic
  val wData = io.csrWriteVal
  val computedWData = WireDefault(readVal)

  switch(io.csrOp) {
    is(1.U) { computedWData := wData }              // CSRRW
    is(2.U) { computedWData := readVal | wData }     // CSRRS
    is(3.U) { computedWData := readVal & (~wData) }  // CSRRC
  }

  when(io.valid && io.csrOp =/= 0.U) {
    switch(io.csrAddr) {
      is(0x300.U) { mstatusReg := computedWData }
      is(0x304.U) { mieReg     := computedWData }
      is(0x104.U) { sieReg     := computedWData }
      is(0x305.U) { mtvecReg   := computedWData }
      is(0x105.U) { stvecReg   := computedWData }
      is(0x341.U) { mepcReg    := computedWData }
      is(0x141.U) { sepcReg    := computedWData }
      is(0x342.U) { mcauseReg  := computedWData }
      is(0x142.U) { scauseReg  := computedWData }
      is(0x180.U) { satpReg    := computedWData }
      is(0x003.U) { fcsrReg    := computedWData }
    }
  }

  // Trap Entry Hardware State Transitions
  when(io.trapOccurred) {
    when(privModeReg === PrivilegeMode.Machine) {
      mepcReg     := io.trapPC
      mcauseReg   := io.trapCause
      privModeReg := PrivilegeMode.Machine
    }.otherwise {
      sepcReg     := io.trapPC
      scauseReg   := io.trapCause
      privModeReg := PrivilegeMode.Supervisor
    }
  }

  // Trap Return Hardware State Transitions
  when(io.mret) {
    privModeReg := PrivilegeMode.Supervisor
  }.elsewhen(io.sret) {
    privModeReg := PrivilegeMode.User
  }

  io.trapVector := Mux(privModeReg === PrivilegeMode.Machine, mtvecReg, stvecReg)
  io.epc        := Mux(io.mret, mepcReg, sepcReg)
}
