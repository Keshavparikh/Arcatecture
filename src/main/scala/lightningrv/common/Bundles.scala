package lightningrv.common

import chisel3._
import chisel3.util._

/**
  * Keshav-ISA Apex Universal Pipeline Data Contracts (v1.0 Frozen Specification)
  */

class MicroOp(implicit config: ApexConfig) extends Bundle {
  val pc              = UInt(64.W)
  val inst            = UInt(32.W)

  // Architectural Register Identifiers
  val rs1             = UInt(5.W)
  val rs2             = UInt(5.W)
  val rd              = UInt(5.W)

  // Physical Register Specifiers (Renamer Output)
  val prs1            = UInt(log2Up(config.PRF_SIZE).W)
  val prs2            = UInt(log2Up(config.PRF_SIZE).W)
  val prd             = UInt(log2Up(config.PRF_SIZE).W)
  val stalePrd        = UInt(log2Up(config.PRF_SIZE).W)

  // In-Flight Allocation Index Tags
  val robIdx          = UInt(log2Up(config.ROB_ENTRIES).W)
  val lqIdx           = UInt(log2Up(config.LQ_ENTRIES).W)
  val sqIdx           = UInt(log2Up(config.SQ_ENTRIES).W)
  val checkpointIdx   = UInt(log2Up(config.MAX_CHECKPOINTS).W)
  val branchMask      = UInt(config.MAX_CHECKPOINTS.W)

  // Operation Type Decodes
  val isBranch        = Bool()
  val isJal           = Bool()
  val isJalr          = Bool()
  val isLoad          = Bool()
  val isStore         = Bool()
  val isAtomic        = Bool()
  val isFp            = Bool()
  val isVector        = Bool()
  val isSystem        = Bool()

  val aluOp           = UInt(4.W)
  val imm             = UInt(64.W)
  val useImm          = Bool()

  // Exception State
  val exceptionValid  = Bool()
  val exceptionCause  = UInt(64.W)
}

class BranchUpdate(implicit config: ApexConfig) extends Bundle {
  val pc             = UInt(64.W)
  val target         = UInt(64.W)
  val taken          = Bool()
  val mispredicted   = Bool()
  val checkpointIdx  = UInt(log2Up(config.MAX_CHECKPOINTS).W)
  val robIdx         = UInt(log2Up(config.ROB_ENTRIES).W)
}

class CommitInfo(implicit config: ApexConfig) extends Bundle {
  val valid          = Bool()
  val pc             = UInt(64.W)
  val robIdx         = UInt(log2Up(config.ROB_ENTRIES).W)
  val prd            = UInt(log2Up(config.PRF_SIZE).W)
  val stalePrd       = UInt(log2Up(config.PRF_SIZE).W)
  val isStore        = Bool()
  val sqIdx          = UInt(log2Up(config.SQ_ENTRIES).W)
  val exceptionValid = Bool()
  val exceptionCause = UInt(64.W)
}

class WritebackInfo(implicit config: ApexConfig) extends Bundle {
  val valid          = Bool()
  val robIdx         = UInt(log2Up(config.ROB_ENTRIES).W)
  val prd            = UInt(log2Up(config.PRF_SIZE).W)
  val data           = UInt(64.W)
}

class RedirectInfo(implicit config: ApexConfig) extends Bundle {
  val valid          = Bool()
  val targetPC       = UInt(64.W)
  val robIdx         = UInt(log2Up(config.ROB_ENTRIES).W)
  val checkpointIdx  = UInt(log2Up(config.MAX_CHECKPOINTS).W)
  val isException    = Bool()
}

class ExceptionInfo extends Bundle {
  val valid          = Bool()
  val cause          = UInt(64.W)
  val epc            = UInt(64.W)
  val badAddr        = UInt(64.W)
}

class MemoryRequest(implicit config: ApexConfig) extends Bundle {
  val valid          = Bool()
  val addr           = UInt(64.W)
  val writeData      = UInt(64.W)
  val writeData256   = UInt(256.W)
  val isWrite        = Bool()
  val isVector       = Bool()
  val size           = UInt(3.W)
  val lqIdx          = UInt(log2Up(config.LQ_ENTRIES).W)
  val sqIdx          = UInt(log2Up(config.SQ_ENTRIES).W)
}

class MemoryResponse(implicit config: ApexConfig) extends Bundle {
  val valid          = Bool()
  val data           = UInt(64.W)
  val data256        = UInt(256.W)
  val isVector       = Bool()
  val lqIdx          = UInt(log2Up(config.LQ_ENTRIES).W)
}
