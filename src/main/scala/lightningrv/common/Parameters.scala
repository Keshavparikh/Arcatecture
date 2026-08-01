package lightningrv.common

import chisel3._

/**
  * Keshav-ISA Apex Architecture Configurations & Parameters (v1.0 Frozen Specification)
  */
case class ApexConfig(
  FETCH_WIDTH: Int          = 4,
  DECODE_WIDTH: Int         = 4,
  RENAME_WIDTH: Int         = 4,
  DISPATCH_WIDTH: Int       = 4,
  ISSUE_WIDTH_INT: Int      = 2,
  ISSUE_WIDTH_MEM: Int      = 1,
  ISSUE_WIDTH_FP: Int       = 1,
  ISSUE_WIDTH_VEC: Int      = 1,
  COMMIT_WIDTH: Int         = 4,
  ROB_ENTRIES: Int          = 32,
  PRF_SIZE: Int             = 96,        // 32 Architectural + 64 Rename Registers
  FREE_LIST_ENTRIES: Int    = 64,
  IQ_SIZE_INT: Int          = 16,
  IQ_SIZE_MEM: Int          = 16,
  IQ_SIZE_FP: Int           = 16,
  IQ_SIZE_VEC: Int          = 16,
  LQ_ENTRIES: Int           = 16,
  SQ_ENTRIES: Int           = 16,
  STORE_BUFFER_ENTRIES: Int = 8,
  VLEN: Int                 = 256,       // 256-Bit SIMD Vector Width
  MSHR_COUNT: Int           = 8,
  CACHE_BANKS: Int          = 4,
  CACHE_LINE_SIZE: Int      = 64,        // 64-Byte Cache Line
  BTB_ENTRIES: Int          = 512,
  RAS_ENTRIES: Int          = 16,
  MAX_CHECKPOINTS: Int      = 8
)
