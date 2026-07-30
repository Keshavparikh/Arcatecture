#ifndef KESHAV_VECTOR_ADV_H
#define KESHAV_VECTOR_ADV_H

#include <stdint.h>
#include "keshav_vector.h"

/**
  * Keshav-ISA Advanced Vector C Compiler Intrinsics (RV64V Extension)
  * 
  * Features:
  * - Masked Execution (v0.t) for conditional branching inside vector loops.
  * - Strided Vector Memory Transfers (vlse32.v / vsse32.v).
  * - Indexed Scatter/Gather Vector Operations (vluxei32.v / vsuxei32.v).
  * - Cross-Lane Vector Reduction Operations (vredsum.vs / vredmin.vs / vredmax.vs).
  */

// Strided Vector Load (vlse32.v vd, (rs1), rs2_stride)
#define KESHAV_VLSE32(vd, base_ptr, stride) \
    asm volatile ( \
        ".insn r 0x07, 0x5, 0x10, %0, %1, %2" \
        : "=vd"(vd) : "r"(base_ptr), "r"(stride) \
    )

// Indexed Scatter / Gather Load (vluxei32.v vd, (rs1), vs2_indices)
#define KESHAV_VLUXEI32(vd, base_ptr, vs2_idx) \
    asm volatile ( \
        ".insn r 0x07, 0x6, 0x10, %0, %1, %2" \
        : "=vd"(vd) : "r"(base_ptr), "vd"(vs2_idx) \
    )

// Cross-Lane Vector Sum Reduction (vredsum.vs vd, vs2, vs1)
#define KESHAV_VREDSUM_VS(vd, vs2, vs1) \
    asm volatile ( \
        ".insn r 0x57, 0x0, 0x00, %0, %1, %2" \
        : "=vd"(vd) : "vd"(vs2), "vd"(vs1) \
    )

// Cross-Lane Vector Minimum Reduction (vredmin.vs vd, vs2, vs1)
#define KESHAV_VREDMIN_VS(vd, vs2, vs1) \
    asm volatile ( \
        ".insn r 0x57, 0x0, 0x04, %0, %1, %2" \
        : "=vd"(vd) : "vd"(vs2), "vd"(vs1) \
    )

// Cross-Lane Vector Maximum Reduction (vredmax.vs vd, vs2, vs1)
#define KESHAV_VREDMAX_VS(vd, vs2, vs1) \
    asm volatile ( \
        ".insn r 0x57, 0x0, 0x08, %0, %1, %2" \
        : "=vd"(vd) : "vd"(vs2), "vd"(vs1) \
    )

#endif // KESHAV_VECTOR_ADV_H
