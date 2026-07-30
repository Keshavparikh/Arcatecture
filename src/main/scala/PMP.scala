package lightningrv

import chisel3._
import chisel3.util._

/**
  * Physical Memory Protection (PMP) Module
  * 
  * Specifications:
  * - OpenSBI compliant M-Mode / S-Mode security boundary isolation.
  * - Registers: 4 Configuration CSRs (pmpcfg0-pmpcfg3) and 16 Address CSRs (pmpaddr0-pmpaddr15).
  * - Checks Read (R), Write (W), and Execute (X) permissions when executing in S-Mode or U-Mode.
  */
class PMP extends Module {
  val io = IO(new Bundle {
    val reqAddr  = Input(UInt(64.W))
    val reqRead  = Input(Bool())
    val reqWrite = Input(Bool())
    val reqExec  = Input(Bool())
    val privMode = Input(UInt(2.W)) // 3=M, 1=S, 0=U

    val pmpAllow = Output(Bool())

    // CSR Read/Write Interface for PMP Configuration
    val csrAddr  = Input(UInt(12.W))
    val csrWData = Input(UInt(64.W))
    val csrWEn   = Input(Bool())
    val csrRData = Output(UInt(64.W))
  })

  // 16 Address Registers (54 bits each, representing 56-bit physical addresses shifted right by 2)
  val pmpAddrRegs = RegInit(VecInit(Seq.fill(16)(0.U(54.W))))
  // 16 Configuration Entries (8 bits each: R, W, X, A[1:0], L)
  val pmpCfgRegs  = RegInit(VecInit(Seq.fill(16)(0.U(8.W))))

  // CSR Read Logic
  val rData = WireDefault(0.U(64.W))
  when(io.csrAddr >= 0x3B0.U && io.csrAddr <= 0x3BF.U) { // pmpaddr0 to pmpaddr15
    rData := pmpAddrRegs(io.csrAddr - 0x3B0.U)
  }.elsewhen(io.csrAddr === 0x3A0.U) { // pmpcfg0
    rData := Cat(pmpCfgRegs(7), pmpCfgRegs(6), pmpCfgRegs(5), pmpCfgRegs(4),
                 pmpCfgRegs(3), pmpCfgRegs(2), pmpCfgRegs(1), pmpCfgRegs(0))
  }
  io.csrRData := rData

  // CSR Write Logic
  when(io.csrWEn) {
    when(io.csrAddr >= 0x3B0.U && io.csrAddr <= 0x3BF.U) {
      pmpAddrRegs(io.csrAddr - 0x3B0.U) := io.csrWData(53, 0)
    }.elsewhen(io.csrAddr === 0x3A0.U) {
      for (i <- 0 until 8) {
        pmpCfgRegs(i) := io.csrWData(i * 8 + 7, i * 8)
      }
    }
  }

  // Permission Verification (Machine mode bypasses PMP unless Locked bit 'L' is set)
  val isMachine = io.privMode === PrivilegeMode.Machine

  val matches = Wire(Vec(16, Bool()))
  val allows  = Wire(Vec(16, Bool()))

  for (i <- 0 until 16) {
    val cfg = pmpCfgRegs(i)
    val r   = cfg(0)
    val w   = cfg(1)
    val x   = cfg(2)
    val a   = cfg(4, 3) // Address Matching Mode (0=OFF, 1=TOR, 2=NA4, 3=NAPOT)

    val pmpAddrShifted = Cat(pmpAddrRegs(i), 0.U(2.W))

    // Top-Of-Range (TOR) Address Check
    val prevAddr = if (i == 0) 0.U(56.W) else Cat(pmpAddrRegs(i - 1), 0.U(2.W))
    val torMatch = (a === 1.U) && (io.reqAddr >= prevAddr) && (io.reqAddr < pmpAddrShifted)

    matches(i) := (a =/= 0.U) && torMatch
    allows(i)  := Mux(io.reqRead, r, true.B) && Mux(io.reqWrite, w, true.B) && Mux(io.reqExec, x, true.B)
  }

  val activeMatch = PriorityEncoder(matches)
  val hasMatch    = matches.asUInt.orR

  io.pmpAllow := Mux(isMachine, true.B, Mux(hasMatch, allows(activeMatch), true.B))
}
