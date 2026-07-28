package lightningrv

import chisel3._
import chisel3.util._

/**
  * AGU (Address Generation Unit / Dual-Issue Decoder & BTB Predictor)
  * 
  * Features:
  * - 64-bit Dual Instruction Fetch Input (imemData64) receiving inst0 and inst1.
  * - Dual Instruction Decoding & Structural / Hazard Detection.
  * - Bank Collision Pre-Filtering: Prevents dual dispatch when both instructions target the same register bank (even/odd).
  * - Control Flow Enforcement: Branch and jump instructions issue in Lane 0 for deterministic target resolution.
  * - Integrated 16-Entry Direct-Mapped 2-Bit Saturating Counter BTB Branch Predictor.
  * - Zero-latency Branch Target Prediction and Speculative PC Increment (+4 or +8).
  * - Pipeline Stall on memory/system fence operations and non-speculative trap halt.
  */
class AGU extends Module {
  val io = IO(new Bundle {
    // 64-bit Instruction Memory Input from Scratchpad
    val imemData64 = Input(UInt(64.W))
    val pc         = Output(UInt(32.W))

    // Dual Decoupled Output Interfaces to ComputeUnit
    val out0 = Decoupled(new ExecuteCommand)
    val out1 = Decoupled(new ExecuteCommand)

    // Pipeline Interlocks & System Trap Halt Signal
    val fenceStall = Input(Bool())
    val trapHaltIn = Input(Bool())

    // BTB Branch Redirection & Training Input from ComputeUnit
    val branchRedirect  = Input(Bool())
    val branchTarget    = Input(UInt(32.W))
    val btbUpdateValid  = Input(Bool())
    val btbUpdatePC     = Input(UInt(32.W))
    val btbUpdateTarget = Input(UInt(32.W))
    val btbUpdateTaken  = Input(Bool())

    // Trap / Halt Output Signal (asserts on ecall)
    val trapHalt = Output(Bool())
  })

  // Program Counter Register (32-Bit)
  val pc = RegInit(0.U(32.W))
  io.pc := pc

  // Instantiate 16-Entry 2-Bit BTB Branch Predictor
  val btb = Module(new BranchPredictor(numEntries = 16))

  // Connect BTB Training Interface from ComputeUnit
  btb.io.updateValid  := io.btbUpdateValid
  btb.io.updatePC     := io.btbUpdatePC
  btb.io.updateTarget := io.btbUpdateTarget
  btb.io.updateTaken  := io.btbUpdateTaken

  // Split 64-bit Instruction Word into 32-bit inst0 and inst1
  val inst0 = io.imemData64(31, 0)
  val inst1 = io.imemData64(63, 32)

  val opcode0 = inst0(6, 0)
  val rd0     = inst0(11, 7)
  val funct30 = inst0(14, 12)
  val rs10    = inst0(19, 15)
  val rs20    = inst0(24, 20)
  val funct70 = inst0(31, 25)

  val opcode1 = inst1(6, 0)
  val rd1     = inst1(11, 7)
  val funct31 = inst1(14, 12)
  val rs11    = inst1(19, 15)
  val rs21    = inst1(24, 20)
  val funct71 = inst1(31, 25)

  val isRType0 = opcode0 === "b0110011".U
  val isIType0 = opcode0 === "b0010011".U || opcode0 === "b0000011".U
  val isSType0 = opcode0 === "b0100011".U
  val isBType0 = opcode0 === "b1100011".U
  val isUType0 = opcode0 === "b0110111".U
  val isJType0 = opcode0 === "b1101111".U
  val isJalr0  = opcode0 === "b1100111".U
  val isFence0 = opcode0 === "b0001111".U || opcode0 === "b1110011".U

  val isRType1 = opcode1 === "b0110011".U
  val isIType1 = opcode1 === "b0010011".U || opcode1 === "b0000011".U
  val isSType1 = opcode1 === "b0100011".U
  val isBType1 = opcode1 === "b1100011".U
  val isUType1 = opcode1 === "b0110111".U
  val isJType1 = opcode1 === "b1101111".U
  val isJalr1  = opcode1 === "b1100111".U
  val isFence1 = opcode1 === "b0001111".U || opcode1 === "b1110011".U

  val isMType0 = isRType0 && funct70 === "b0000001".U
  val isMType1 = isRType1 && funct71 === "b0000001".U

  val isMemLane0 = opcode0 === "b0000011".U || opcode0 === "b0100011".U
  val isMemLane1 = opcode1 === "b0000011".U || opcode1 === "b0100011".U

  val isFastLane0 = !isMType0 && !isMemLane0
  val isFastLane1 = !isMType1 && !isMemLane1

  val iImm0 = Cat(Fill(20, inst0(31)), inst0(31, 20))
  val sImm0 = Cat(Fill(20, inst0(31)), inst0(31, 25), inst0(11, 7))
  val bImm0 = Cat(Fill(20, inst0(31)), inst0(7), inst0(30, 25), inst0(11, 8), 0.U(1.W))
  val uImm0 = Cat(inst0(31, 12), 0.U(12.W))
  val jImm0 = Cat(Fill(12, inst0(31)), inst0(19, 12), inst0(20), inst0(30, 21), 0.U(1.W))

  val useImm0 = isIType0 || isSType0 || isBType0 || isUType0 || isJType0
  val imm0    = Mux(isSType0, sImm0, Mux(isBType0, bImm0, Mux(isUType0, uImm0, Mux(isJType0, jImm0, iImm0))))

  val iImm1 = Cat(Fill(20, inst1(31)), inst1(31, 20))
  val sImm1 = Cat(Fill(20, inst1(31)), inst1(31, 25), inst1(11, 7))
  val bImm1 = Cat(Fill(20, inst1(31)), inst1(7), inst1(30, 25), inst1(11, 8), 0.U(1.W))
  val uImm1 = Cat(inst1(31, 12), 0.U(12.W))
  val jImm1 = Cat(Fill(12, inst1(31)), inst1(19, 12), inst1(20), inst1(30, 21), 0.U(1.W))

  val useImm1 = isIType1 || isSType1 || isBType1 || isUType1 || isJType1
  val imm1    = Mux(isSType1, sImm1, Mux(isBType1, bImm1, Mux(isUType1, uImm1, Mux(isJType1, jImm1, iImm1))))

  val aluOp0 = WireDefault(ALUOp.ADD)
  when(isMType0) {
    switch(funct30) {
      is("b000".U) { aluOp0 := ALUOp.MUL }
      is("b001".U) { aluOp0 := ALUOp.MULH }
      is("b100".U) { aluOp0 := ALUOp.DIV }
      is("b110".U) { aluOp0 := ALUOp.REM }
    }
  }.elsewhen(isRType0 || isIType0) {
    switch(funct30) {
      is("b000".U) { aluOp0 := Mux(isRType0 && funct70(5), ALUOp.SUB, Mux(isIType0, ALUOp.ADDI, ALUOp.ADD)) }
      is("b001".U) { aluOp0 := ALUOp.SLL }
      is("b010".U) { aluOp0 := ALUOp.SLT }
      is("b011".U) { aluOp0 := ALUOp.SLTU }
      is("b100".U) { aluOp0 := ALUOp.XOR }
      is("b101".U) { aluOp0 := Mux(funct70(5), ALUOp.SRA, ALUOp.SRL) }
      is("b110".U) { aluOp0 := ALUOp.OR }
      is("b111".U) { aluOp0 := ALUOp.AND }
    }
  }.elsewhen(isUType0) {
    aluOp0 := ALUOp.LUI
  }

  val aluOp1 = WireDefault(ALUOp.ADD)
  when(isMType1) {
    switch(funct31) {
      is("b000".U) { aluOp1 := ALUOp.MUL }
      is("b001".U) { aluOp1 := ALUOp.MULH }
      is("b100".U) { aluOp1 := ALUOp.DIV }
      is("b110".U) { aluOp1 := ALUOp.REM }
    }
  }.elsewhen(isRType1 || isIType1) {
    switch(funct31) {
      is("b000".U) { aluOp1 := Mux(isRType1 && funct71(5), ALUOp.SUB, Mux(isIType1, ALUOp.ADDI, ALUOp.ADD)) }
      is("b001".U) { aluOp1 := ALUOp.SLL }
      is("b010".U) { aluOp1 := ALUOp.SLT }
      is("b011".U) { aluOp1 := ALUOp.SLTU }
      is("b100".U) { aluOp1 := ALUOp.XOR }
      is("b101".U) { aluOp1 := Mux(funct71(5), ALUOp.SRA, ALUOp.SRL) }
      is("b110".U) { aluOp1 := ALUOp.OR }
      is("b111".U) { aluOp1 := ALUOp.AND }
    }
  }.elsewhen(isUType1) {
    aluOp1 := ALUOp.LUI
  }

  btb.io.fetchPC := pc

  val cmd0 = Wire(new ExecuteCommand)
  cmd0.pc              := pc
  cmd0.inst            := inst0
  cmd0.rd              := rd0
  cmd0.rs1             := rs10
  cmd0.rs2             := rs20
  cmd0.funct3          := funct30
  cmd0.useImm          := useImm0
  cmd0.imm             := imm0
  cmd0.aluOp           := aluOp0
  cmd0.isSlowLane      := isMType0
  cmd0.isFastLane      := isFastLane0
  cmd0.isMemLane       := isMemLane0
  cmd0.isLoad          := opcode0 === "b0000011".U
  cmd0.isStore         := opcode0 === "b0100011".U
  cmd0.isBranch        := isBType0
  cmd0.isJump          := isJType0 || isJalr0
  cmd0.isJalr          := isJalr0
  cmd0.isFence         := isFence0
  cmd0.predictedTaken  := btb.io.predictTaken
  cmd0.predictedTarget := btb.io.predictTarget

  val cmd1 = Wire(new ExecuteCommand)
  cmd1.pc              := pc + 4.U
  cmd1.inst            := inst1
  cmd1.rd              := rd1
  cmd1.rs1             := rs11
  cmd1.rs2             := rs21
  cmd1.funct3          := funct31
  cmd1.useImm          := useImm1
  cmd1.imm             := imm1
  cmd1.aluOp           := aluOp1
  cmd1.isSlowLane      := isMType1
  cmd1.isFastLane      := isFastLane1
  cmd1.isMemLane       := isMemLane1
  cmd1.isLoad          := opcode1 === "b0000011".U
  cmd1.isStore         := opcode1 === "b0100011".U
  cmd1.isBranch        := isBType1
  cmd1.isJump          := isJType1 || isJalr1
  cmd1.isJalr          := isJalr1
  cmd1.isFence         := isFence1
  cmd1.predictedTaken  := false.B
  cmd1.predictedTarget := 0.U

  val isRawHazardBetweenDualInsts = rs11 =/= 0.U && rd0 =/= 0.U && rs11 === rd0 ||
                                    rs21 =/= 0.U && rd0 =/= 0.U && rs21 === rd0

  val isWawHazardBetweenDualInsts = rd0 =/= 0.U && rd1 =/= 0.U && rd0 === rd1

  val isWarHazardBetweenDualInsts = rd1 =/= 0.U && (rs10 === rd1 || rs20 === rd1)

  // Structural Bank Write Collision: Both instructions writing to the same register bank (even/odd)
  val isBankCollision = rd0 =/= 0.U && rd1 =/= 0.U && (rd0(0) === rd1(0))

  val isSameQueueHazard = (isMType0 && isMType1) ||
                          (isMemLane0 && isMemLane1)

  // Control Flow Enforcement: Branch and jump instructions issue in Lane 0
  val isControlFlowInDualInsts = isBType0 || isJType0 || isJalr0 || isFence0 ||
                                isBType1 || isJType1 || isJalr1 || isFence1

  val canDualIssue = !isRawHazardBetweenDualInsts &&
                     !isWawHazardBetweenDualInsts &&
                     !isWarHazardBetweenDualInsts &&
                     !isBankCollision             &&
                     !isSameQueueHazard           &&
                     !isControlFlowInDualInsts

  val stall = io.fenceStall || io.trapHaltIn

  io.out0.valid := !stall && (opcode0 =/= 0.U)
  io.out0.bits  := cmd0

  io.out1.valid := !stall && canDualIssue && (opcode1 =/= 0.U)
  io.out1.bits  := cmd1

  val pcNext = WireDefault(pc + 8.U)

  when(io.branchRedirect) {
    pcNext := io.branchTarget
  }.elsewhen(stall) {
    pcNext := pc
  }.elsewhen(isBType0 && btb.io.predictTaken) {
    pcNext := btb.io.predictTarget
  }.elsewhen(isJType0) {
    pcNext := pc + jImm0
  }.elsewhen(canDualIssue && io.out0.ready && io.out1.ready) {
    pcNext := pc + 8.U
  }.elsewhen(io.out0.ready) {
    pcNext := pc + 4.U
  }

  io.trapHalt := io.trapHaltIn

  when(!io.trapHaltIn) {
    pc := pcNext
  }
}
