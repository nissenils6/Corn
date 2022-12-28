format pe64 console
entry start

section '.text' code readable executable

println_i64:
        mov     r8, rsp
        sub     rsp, 96

        sub     r8, 1
        mov     byte[r8], 10

        mov     r10, rax
        mov     rcx, 10
        cmp     rax, 0
        jge     println_i64_loop
        neg     rax

println_i64_loop:
        xor     rdx, rdx
        div     rcx
        add     rdx, 48
        sub     r8, 1
        mov     byte[r8], dl
        cmp     rax, 0
        jg      println_i64_loop

        cmp     r10, 0
        jge     println_i64_positive
        sub     r8, 1
        mov     byte[r8], '-'

println_i64_positive:
        mov     rcx, qword[stdout]
        mov     rdx, r8
        lea     r8, [rsp + 96]
        sub     r8, rdx
        lea     r9, [rsp + 64]
        mov     qword[rsp + 32], 0
        call    qword[WriteFile]

        add     rsp, 96
        ret

input_i64:
        sub     rsp, 96

        mov     rcx, qword[stdin]
        lea     rdx, [rsp + 72]
        mov     r8, 24
        lea     r9, [rsp + 64]
        mov     qword[rsp + 32], 0
        call    qword[ReadFile]

        lea     r8, [rsp + 72]
        mov     rcx, qword[rsp + 64]
        add     rcx, r8
        sub     rcx, 2

        xor    rax, rax
        xor     r9, r9
        movzx   rdx, byte[r8]
        cmp     rdx, '-'
        sete    r9l
        add     r8, r9
        add     r9, r9
        sub     r9, 1
        neg     r9

input_i64_loop:
        movzx   rdx, byte[r8]
        add     r8, 1
        sub     rdx, 48
        add     rax, rax
        lea     rax, [rax * 4 + rax]
        add     rax, rdx
        cmp     r8, rcx
        jb      input_i64_loop

        imul    rax, r9

        add     rsp, 96
        ret

runtime_init:
        sub     rsp, 48

        mov     rcx, -10
        call    qword[GetStdHandle]
        mov     qword[stdin], rax

        mov     rcx, -11
        call    qword[GetStdHandle]
        mov     qword[stdout], rax

        mov     rcx, -12
        call    qword[GetStdHandle]
        mov     qword[stderr], rax

        add     rsp, 48
        ret

start:
        sub     rsp, 8
        call    runtime_init

        call    ;#[ENTRY]

        xor     rcx, rcx
        call    qword[ExitProcess]
        ret

;#[CODE]

section '.data' data readable writeable

stdin:
        dq      0
stdout:
        dq      0
stderr:
        dq      0

;#[DATA]

section '.idata' import data readable writeable

        dd      0, 0, 0, RVA kernel_name, RVA kernel_table, 0, 0, 0, 0, 0

kernel_table:
ReadFile:
        dq      RVA _ReadFile
WriteFile:
        dq      RVA _WriteFile
GetStdHandle:
        dq      RVA _GetStdHandle
ExitProcess:
        dq      RVA _ExitProcess
        dq      0

kernel_name:
        db      'KERNEL32.DLL', 0
user_name:
        db      'USER32.DLL', 0

_ReadFile:
        db      0, 0, 'ReadFile', 0
_WriteFile:
        db      0, 0, 'WriteFile', 0
_GetStdHandle:
        db      0, 0, 'GetStdHandle', 0
_ExitProcess:
        db      0, 0, 'ExitProcess', 0
