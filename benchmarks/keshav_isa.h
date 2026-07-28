#ifndef KESHAV_ISA_H
#define KESHAV_ISA_H

/**
  * Keshav-ISA Compiler Intrinsics Header
  * Standard RISC-V CUSTOM_0 Opcode Space (0b0001011 = 0x0B)
  */

// Fused Shift-Add: rd = rs1 + rs2
#define KESHAV_SADD(rd, rs1, rs2) \
    asm volatile ( \
        ".insn r 0x0B, 0x0, 0x0, %0, %1, %2" \
        : "=r"(rd) : "r"(rs1), "r"(rs2) \
    )

// Hardware Min Compare: rd = (rs1 < rs2) ? rs1 : rs2
#define KESHAV_MIN(rd, rs1, rs2) \
    asm volatile ( \
        ".insn r 0x0B, 0x1, 0x0, %0, %1, %2" \
        : "=r"(rd) : "r"(rs1), "r"(rs2) \
    )

// Hardware Max Compare: rd = (rs1 > rs2) ? rs1 : rs2
#define KESHAV_MAX(rd, rs1, rs2) \
    asm volatile ( \
        ".insn r 0x0B, 0x2, 0x0, %0, %1, %2" \
        : "=r"(rd) : "r"(rs1), "r"(rs2) \
    )

#endif // KESHAV_ISA_H
