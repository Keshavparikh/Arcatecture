package lightningrv.rename

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Rename Map Table (RAT) Module
  * Maps 32 architectural registers (x0-x31) to 96 physical registers (p0-p95).
  */
class RenameMapTable(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    // 4 Instructions Rename Inputs
    val renReq      = Input(Vec(config.RENAME_WIDTH, Bool()))
    val rs1         = Input(Vec(config.RENAME_WIDTH, UInt(5.W)))
    val rs2         = Input(Vec(config.RENAME_WIDTH, UInt(5.W)))
    val rd          = Input(Vec(config.RENAME_WIDTH, UInt(5.W)))
    val allocPrf    = Input(Vec(config.RENAME_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))

    // Renamed Specifier Outputs
    val prs1        = Output(Vec(config.RENAME_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))
    val prs2        = Output(Vec(config.RENAME_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))
    val prd         = Output(Vec(config.RENAME_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))
    val stalePrd    = Output(Vec(config.RENAME_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))

    // Checkpoint Recovery Interface
    val restoreValid= Input(Bool())
    val restoreMap  = Input(Vec(32, UInt(log2Up(config.PRF_SIZE).W)))
    val currentMap  = Output(Vec(32, UInt(log2Up(config.PRF_SIZE).W)))
  })

  // Architectural Register Map Table (Initially x0->p0, x1->p1, ..., x31->p31)
  val mapTable = RegInit(VecInit((0 until 32).map(_.U(log2Up(config.PRF_SIZE).W))))
  io.currentMap := mapTable

  // Intra-cycle dependency resolution across 4 rename slots
  for (i <- 0 until config.RENAME_WIDTH) {
    // Read RS1 mapping with intra-bundle forwarding
    val rs1Map = WireDefault(mapTable(io.rs1(i)))
    for (j <- 0 until i) {
      when(io.renReq(j) && (io.rd(j) === io.rs1(i)) && (io.rd(j) =/= 0.U)) {
        rs1Map := io.allocPrf(j)
      }
    }
    io.prs1(i) := Mux(io.rs1(i) === 0.U, 0.U, rs1Map)

    // Read RS2 mapping with intra-bundle forwarding
    val rs2Map = WireDefault(mapTable(io.rs2(i)))
    for (j <- 0 until i) {
      when(io.renReq(j) && (io.rd(j) === io.rs2(i)) && (io.rd(j) =/= 0.U)) {
        rs2Map := io.allocPrf(j)
      }
    }
    io.prs2(i) := Mux(io.rs2(i) === 0.U, 0.U, rs2Map)

    // Stale PRD for ROB allocation
    io.stalePrd(i) := Mux(io.rd(i) === 0.U, 0.U, mapTable(io.rd(i)))
    io.prd(i)      := Mux(io.rd(i) === 0.U, 0.U, io.allocPrf(i))
  }

  // Update Map Table
  when(io.restoreValid) {
    mapTable := io.restoreMap
  }.otherwise {
    for (i <- 0 until config.RENAME_WIDTH) {
      when(io.renReq(i) && (io.rd(i) =/= 0.U)) {
        mapTable(io.rd(i)) := io.allocPrf(i)
      }
    }
  }
}
