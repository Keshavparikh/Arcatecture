#ifndef KESHAV_VECTOR_H
#define KESHAV_VECTOR_H

#include <stdint.h>
#include "keshav_isa.h"

/**
  * Keshav-ISA 8-Wide SIMD Vector Compiler Intrinsics Header (RV64V)
  * Standard RISC-V Vector Opcode Space (OP_VECTOR = 0b1010111 = 0x57)
  */

// 256-Bit Vector Load: Loads 8 x 32-bit elements from memory in 1 cycle
#define KESHAV_VLE32(vd, base_ptr) \
    asm volatile ( \
        ".insn i 0x07, 0x0, v" #vd ", (%0), 0" \
        : : "r"(base_ptr) \
    )

// 256-Bit Vector Store: Stores 8 x 32-bit elements to memory in 1 cycle
#define KESHAV_VSE32(vs2, base_ptr) \
    asm volatile ( \
        ".insn s 0x27, 0x0, v" #vs2 ", (%0), 0" \
        : : "r"(base_ptr) \
    )

// 8-Lane Parallel Vector Addition: vd = vs2 + vs1 (8 elements in 1 cycle)
#define KESHAV_VADD_VV(vd, vs2, vs1) \
    asm volatile ( \
        ".insn r 0x57, 0x0, 0x00, v" #vd ", v" #vs1 ", v" #vs2 \
    )

// 8-Lane Parallel Vector Signed Min: vd = min(vs2, vs1) (8 elements in 1 cycle)
#define KESHAV_VMIN_VV(vd, vs2, vs1) \
    asm volatile ( \
        ".insn r 0x57, 0x0, 0x04, v" #vd ", v" #vs1 ", v" #vs2 \
    )

// 8-Lane Parallel Vector Signed Max: vd = max(vs2, vs1) (8 elements in 1 cycle)
#define KESHAV_VMAX_VV(vd, vs2, vs1) \
    asm volatile ( \
        ".insn r 0x57, 0x0, 0x05, v" #vd ", v" #vs1 ", v" #vs2 \
    )

#endif // KESHAV_VECTOR_H
