/* Linker script for LightningRV Bare-Metal C Benchmarks */
OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{
    . = 0x00000000;
    .text : {
        *(.text._start)
        *(.text*)
    }

    .rodata : {
        *(.rodata*)
    }

    /* Place data section at 2KB boundary (word index 512) to separate code and data */
    . = 0x00000800;
    .data : {
        *(.data*)
        *(.sdata*)
    }

    .bss : {
        *(.bss*)
        *(.sbss*)
    }

    _end = .;
}
