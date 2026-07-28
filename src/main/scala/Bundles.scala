package lightningrv

import chisel3._

/**
  * Decoded Instruction Command Bundle for Pipeline Decoupled Queues
  */
class ExecuteCommand extends Bundle {
  val pc              = UInt(32.W)
  val inst            = UInt(32.W)
  val rd              = UInt(5.W)
  val rs1             = UInt(5.W)
  val rs2             = UInt(5.W)
  val funct3          = UInt(3.W)
  val useImm          = Bool()
  val imm             = UInt(32.W)
  val aluOp           = UInt(4.W)
  val isSlowLane      = Bool()
  val isFastLane      = Bool()
  val isMemLane       = Bool()
  val isLoad          = Bool()
  val isStore         = Bool()
  val isBranch        = Bool()
  val isJump          = Bool()
  val isJalr          = Bool()
  val isFence         = Bool()
  val predictedTaken  = Bool()
  val predictedTarget = UInt(32.W)
}

object ALUOp {
  val ADD  = 0.U(4.W)
  val SUB  = 1.U(4.W)
  val ADDI = 2.U(4.W)
  val SLL  = 3.U(4.W)
  val SRL  = 4.U(4.W)
  val SRA  = 5.U(4.W)
  val SLT  = 6.U(4.W)
  val SLTU = 7.U(4.W)
  val XOR  = 8.U(4.W)
  val OR   = 9.U(4.W)
  val AND  = 10.U(4.W)
  val LUI  = 11.U(4.W)
  val MUL  = 12.U(4.W)
  val MULH = 13.U(4.W)
  val DIV  = 14.U(4.W)
  val REM  = 15.U(4.W)
}
