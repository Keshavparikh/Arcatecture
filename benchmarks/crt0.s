.section .text._start
.global _start
_start:
    li sp, 4096       # Initialize stack pointer at 4KB boundary (above 2KB .data segment)
    call main         # Call C main() entrypoint (returns result in register a0 / x10)
    ecall             # Signal system trap halt to LightningRV core
halt_loop:
    j halt_loop
