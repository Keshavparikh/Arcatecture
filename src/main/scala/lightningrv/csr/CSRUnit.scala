package lightningrv.csr

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Privilege Control and Status Register Unit (CSRUnit)
  * Machine (M-Mode = 3), Supervisor (S-Mode = 1), User (U-Mode = 0) State Machine.
  */
class CSRUnit(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val csrAddr    = Input(UInt(12.W))
    val csrWriteVal= Input(UInt(64.W))
    val csrOp      = Input(UInt(2.W))
    val valid      = Input(Bool())

    val privMode   = Output(UInt(2.W))
    val satp       = Output(UInt(64.W))
    val csrReadVal = Output(UInt(64.W))

    val trapValid  = Input(Bool())
    val trapCause  = Input(UInt(64.W))
    val trapPC     = Input(UInt(64.W))
    val mret       = Input(Bool())
    val sret       = Input(Bool())

    val trapVector = Output(UInt(64.W))
    val epc        = Output(UInt(64.W))
  })

  val privReg    = RegInit(3.U(2.W))
  val mstatusReg = RegInit(0.U(64.W))
  val mtvecReg   = RegInit(0.U(64.W))
  val mepcReg    = RegInit(0.U(64.W))
  val mcauseReg  = RegInit(0.U(64.W))
  val satpReg    = RegInit(0.U(64.W))

  io.privMode := privReg
  io.satp     := satpReg

  val rVal = WireDefault(0.U(64.W))
  switch(io.csrAddr) {
    is(0x300.U) { rVal := mstatusReg }
    is(0x305.U) { rVal := mtvecReg }
    is(0x341.U) { rVal := mepcReg }
    is(0x342.U) { rVal := mcauseReg }
    is(0x180.U) { rVal := satpReg }
  }
  io.csrReadVal := rVal

  val wData = io.csrWriteVal
  when(io.valid && io.csrOp =/= 0.U) {
    switch(io.csrAddr) {
      is(0x300.U) { mstatusReg := wData }
      is(0x305.U) { mtvecReg   := wData }
      is(0x341.U) { mepcReg    := wData }
      is(0x342.U) { mcauseReg  := wData }
      is(0x180.U) { satpReg    := wData }
    }
  }

  when(io.trapValid) {
    mepcReg   := io.trapPC
    mcauseReg := io.trapCause
    privReg   := 3.U // Enter Machine mode on trap
  }

  when(io.mret) {
    privReg := 1.U // Return to Supervisor mode
  }.elsewhen(io.sret) {
    privReg := 0.U // Return to User mode
  }

  io.trapVector := mtvecReg
  io.epc        := mepcReg
}
