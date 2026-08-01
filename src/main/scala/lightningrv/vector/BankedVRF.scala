package lightningrv.vector

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * 8-Banked Vector Register File (BankedVRF)
  * Splits 32 x 256-bit vector registers into 8 physical 32-bit SRAM banks to resolve routing congestion.
  */
class BankedVRF(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val vs1         = Input(UInt(5.W))
    val vs2         = Input(UInt(5.W))
    val vs1Data     = Output(UInt(256.W))
    val vs2Data     = Output(UInt(256.W))
    val v0Mask      = Output(UInt(8.W))

    val vd          = Input(UInt(5.W))
    val writeEnable = Input(Bool())
    val writeData   = Input(UInt(256.W))

    val vectorRegs  = Output(Vec(32, UInt(256.W)))
  })

  // 8 Physical Memory Banks (32 registers x 32 bits each)
  val bank0 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val bank1 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val bank2 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val bank3 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val bank4 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val bank5 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val bank6 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val bank7 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  io.vs1Data := Cat(bank7(io.vs1), bank6(io.vs1), bank5(io.vs1), bank4(io.vs1),
                    bank3(io.vs1), bank2(io.vs1), bank1(io.vs1), bank0(io.vs1))

  io.vs2Data := Cat(bank7(io.vs2), bank6(io.vs2), bank5(io.vs2), bank4(io.vs2),
                    bank3(io.vs2), bank2(io.vs2), bank1(io.vs2), bank0(io.vs2))

  io.v0Mask  := bank0(0)(7, 0)

  when(io.writeEnable) {
    bank0(io.vd) := io.writeData(31, 0)
    bank1(io.vd) := io.writeData(63, 32)
    bank2(io.vd) := io.writeData(95, 64)
    bank3(io.vd) := io.writeData(127, 96)
    bank4(io.vd) := io.writeData(159, 128)
    bank5(io.vd) := io.writeData(191, 160)
    bank6(io.vd) := io.writeData(223, 192)
    bank7(io.vd) := io.writeData(255, 224)
  }

  val vregs = Wire(Vec(32, UInt(256.W)))
  for (r <- 0 until 32) {
    vregs(r) := Cat(bank7(r), bank6(r), bank5(r), bank4(r), bank3(r), bank2(r), bank1(r), bank0(r))
  }
  io.vectorRegs := vregs
}
