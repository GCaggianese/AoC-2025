;;; SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
;;;
;;; SPDX-License-Identifier: Apache-2.0

format ELF64 executable
entry _start

macro matrix_from_file name, filename {
    virtual at 0
        name#.data:: file filename
        name#.size = $
    end virtual
    label name byte
    name#.len = 0
    repeat name#.size
        load name#.ch byte from name#.data:(%-1)
        if name#.ch = '.'
            db 1
            name#.len = name#.len + 1
        else if name#.ch = '@'
            db 0
            name#.len = name#.len + 1
        end if
    end repeat
}

segment readable writeable
    matrix_from_file my_matrix, './input.txt'
    MATRIX_SIZE = my_matrix.len
    ROWS = 138
    COLS = 138

    result rb ROWS * COLS

    directions db -1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1
    NUM_DIRS = 8

    ; For printing
    msg db 'Counter: '
    msg_len = $ - msg
    buffer rb 16
    newline db 0x0A

segment readable executable

_start:
    xor r12, r12

.row_loop:
    cmp r12, ROWS
    jge .correction

    xor r13, r13

.col_loop:
    cmp r13, COLS
    jge .next_row

    mov rax, r12
    imul rax, COLS
    add rax, r13
    movzx r14d, byte [my_matrix + rax]

    xor r15, r15

.dir_loop:
    cmp r15, NUM_DIRS
    jge .store_result

    movsx r8, byte [directions + r15*2]
    movsx r9, byte [directions + r15*2 + 1]

    mov r10, r12
    add r10, r8
    mov r11, r13
    add r11, r9

    cmp r10, 0
    jl .next_dir
    cmp r10, ROWS
    jge .next_dir
    cmp r11, 0
    jl .next_dir
    cmp r11, COLS
    jge .next_dir

    mov rax, r10
    imul rax, COLS
    add rax, r11
    movzx eax, byte [my_matrix + rax]
    add r14, rax

.next_dir:
    inc r15
    jmp .dir_loop

.store_result:
    mov rax, r12
    imul rax, COLS
    add rax, r13
    mov byte [result + rax], r14b

    inc r13
    jmp .col_loop

.next_row:
    inc r12
    jmp .row_loop

.correction:
    xor rcx, rcx
.top_row:
    cmp rcx, COLS
    jge .bottom_row
    add byte [result + rcx], 3
    inc rcx
    jmp .top_row

.bottom_row:
    xor rcx, rcx
.bottom_loop:
    cmp rcx, COLS
    jge .left_col
    mov rax, (ROWS-1) * COLS
    add rax, rcx
    add byte [result + rax], 3
    inc rcx
    jmp .bottom_loop

.left_col:
    mov rcx, 1
.left_loop:
    cmp rcx, ROWS-1
    jge .right_col
    mov rax, rcx
    imul rax, COLS
    add byte [result + rax], 3
    inc rcx
    jmp .left_loop

.right_col:
    mov rcx, 1
.right_loop:
    cmp rcx, ROWS-1
    jge .corners
    mov rax, rcx
    imul rax, COLS
    add rax, COLS-1
    add byte [result + rax], 3
    inc rcx
    jmp .right_loop

.corners:
    add byte [result], 2
    add byte [result + COLS - 1], 2
    add byte [result + (ROWS-1) * COLS], 2
    add byte [result + (ROWS-1) * COLS + COLS - 1], 2

    xor rcx, rcx
.filter_loop:
    cmp rcx, MATRIX_SIZE
    jge .count

    cmp byte [my_matrix + rcx], 1
    jne .filter_next
    mov byte [result + rcx], 0

.filter_next:
    inc rcx
    jmp .filter_loop

.count:
    xor r12, r12            ; r12 = counter
    xor rcx, rcx

.count_loop:
    cmp rcx, MATRIX_SIZE
    jge .print_result

    cmp byte [result + rcx], 5
    jl .count_next
    inc r12

.count_next:
    inc rcx
    jmp .count_loop

.print_result:
    mov rax, 1
    mov rdi, 1
    mov rsi, msg
    mov rdx, msg_len
    syscall

    mov rax, r12
    lea rdi, [buffer + 15]
    mov byte [rdi], 0
    mov rcx, 10

    test rax, rax
    jnz .convert_loop

    dec rdi
    mov byte [rdi], '0'
    jmp .print_number

.convert_loop:
    test rax, rax
    jz .print_number
    xor rdx, rdx
    div rcx
    add dl, '0'
    dec rdi
    mov [rdi], dl
    jmp .convert_loop

.print_number:
    lea rdx, [buffer + 15]
    sub rdx, rdi
    mov rsi, rdi
    mov rax, 1
    mov rdi, 1
    syscall

    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall

byebye:
    ;; 下次見～
    mov rax, 60
    mov rdi, 0
    syscall
