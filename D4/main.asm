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
    backup rb ROWS * COLS

    directions db -1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1
    NUM_DIRS = 8

    counter_p1 dq 0
    counter_p2 dq 0

    msg_p1 db 'Counter P1: '
    msg_p1_len = $ - msg_p1
    msg_p2 db 'Counter P2: '
    msg_p2_len = $ - msg_p2
    buffer rb 16
    newline db 0x0A

segment readable executable

_start:

    xor rcx, rcx
.backup_loop:
    cmp rcx, MATRIX_SIZE
    jge .part1_start
    mov al, [my_matrix + rcx]
    mov [backup + rcx], al
    inc rcx
    jmp .backup_loop

.part1_start:
    xor r12, r12

.p1_row_loop:
    cmp r12, ROWS
    jge .p1_correction

    xor r13, r13

.p1_col_loop:
    cmp r13, COLS
    jge .p1_next_row

    mov rax, r12
    imul rax, COLS
    add rax, r13
    movzx r14d, byte [my_matrix + rax]

    xor r15, r15

.p1_dir_loop:
    cmp r15, NUM_DIRS
    jge .p1_store_result

    movsx r8, byte [directions + r15*2]      ; r8 = di
    movsx r9, byte [directions + r15*2 + 1]  ; r9 = dj

    mov r10, r12
    add r10, r8
    mov r11, r13
    add r11, r9

    cmp r10, 0
    jl .p1_next_dir
    cmp r10, ROWS
    jge .p1_next_dir
    cmp r11, 0
    jl .p1_next_dir
    cmp r11, COLS
    jge .p1_next_dir

    mov rax, r10
    imul rax, COLS
    add rax, r11
    movzx eax, byte [my_matrix + rax]
    add r14, rax

.p1_next_dir:
    inc r15
    jmp .p1_dir_loop

.p1_store_result:
    mov rax, r12
    imul rax, COLS
    add rax, r13
    mov byte [result + rax], r14b

    inc r13
    jmp .p1_col_loop

.p1_next_row:
    inc r12
    jmp .p1_row_loop

.p1_correction:
    xor rcx, rcx
.p1_top_row:
    cmp rcx, COLS
    jge .p1_bottom_row
    add byte [result + rcx], 3
    inc rcx
    jmp .p1_top_row

.p1_bottom_row:
    xor rcx, rcx
.p1_bottom_loop:
    cmp rcx, COLS
    jge .p1_left_col
    mov rax, (ROWS-1) * COLS
    add rax, rcx
    add byte [result + rax], 3
    inc rcx
    jmp .p1_bottom_loop

.p1_left_col:
    mov rcx, 1
.p1_left_loop:
    cmp rcx, ROWS-1
    jge .p1_right_col
    mov rax, rcx
    imul rax, COLS
    add byte [result + rax], 3
    inc rcx
    jmp .p1_left_loop

.p1_right_col:
    mov rcx, 1
.p1_right_loop:
    cmp rcx, ROWS-1
    jge .p1_corners
    mov rax, rcx
    imul rax, COLS
    add rax, COLS-1
    add byte [result + rax], 3
    inc rcx
    jmp .p1_right_loop

.p1_corners:
    add byte [result], 2                              ; [0][0]
    add byte [result + COLS - 1], 2                   ; [0][COLS-1]
    add byte [result + (ROWS-1) * COLS], 2            ; [ROWS-1][0]
    add byte [result + (ROWS-1) * COLS + COLS - 1], 2 ; [ROWS-1][COLS-1]

    xor rcx, rcx
.p1_filter_loop:
    cmp rcx, MATRIX_SIZE
    jge .p1_count

    cmp byte [my_matrix + rcx], 1
    jne .p1_filter_next
    mov byte [result + rcx], 0

.p1_filter_next:
    inc rcx
    jmp .p1_filter_loop

.p1_count:
    xor r12, r12
    xor rcx, rcx

.p1_count_loop:
    cmp rcx, MATRIX_SIZE
    jge .p1_done

    cmp byte [result + rcx], 5
    jl .p1_count_next
    inc r12

.p1_count_next:
    inc rcx
    jmp .p1_count_loop

.p1_done:
    mov [counter_p1], r12

    xor rcx, rcx
.restore_loop:
    cmp rcx, MATRIX_SIZE
    jge .p2_main_loop
    mov al, [backup + rcx]
    mov [my_matrix + rcx], al
    inc rcx
    jmp .restore_loop

    mov qword [counter_p2], 0

.p2_main_loop:

    xor r12, r12

.p2_row_loop:
    cmp r12, ROWS
    jge .p2_correction

    xor r13, r13

.p2_col_loop:
    cmp r13, COLS
    jge .p2_next_row

    mov rax, r12
    imul rax, COLS
    add rax, r13
    movzx r14d, byte [my_matrix + rax]

    xor r15, r15

.p2_dir_loop:
    cmp r15, NUM_DIRS
    jge .p2_store_result

    movsx r8, byte [directions + r15*2]
    movsx r9, byte [directions + r15*2 + 1]

    mov r10, r12
    add r10, r8
    mov r11, r13
    add r11, r9

    cmp r10, 0
    jl .p2_next_dir
    cmp r10, ROWS
    jge .p2_next_dir
    cmp r11, 0
    jl .p2_next_dir
    cmp r11, COLS
    jge .p2_next_dir

    mov rax, r10
    imul rax, COLS
    add rax, r11
    movzx eax, byte [my_matrix + rax]
    add r14, rax

.p2_next_dir:
    inc r15
    jmp .p2_dir_loop

.p2_store_result:
    mov rax, r12
    imul rax, COLS
    add rax, r13
    mov byte [result + rax], r14b

    inc r13
    jmp .p2_col_loop

.p2_next_row:
    inc r12
    jmp .p2_row_loop

.p2_correction:
    xor rcx, rcx
.p2_top_row:
    cmp rcx, COLS
    jge .p2_bottom_row
    add byte [result + rcx], 3
    inc rcx
    jmp .p2_top_row

.p2_bottom_row:
    xor rcx, rcx
.p2_bottom_loop:
    cmp rcx, COLS
    jge .p2_left_col
    mov rax, (ROWS-1) * COLS
    add rax, rcx
    add byte [result + rax], 3
    inc rcx
    jmp .p2_bottom_loop

.p2_left_col:
    mov rcx, 1
.p2_left_loop:
    cmp rcx, ROWS-1
    jge .p2_right_col
    mov rax, rcx
    imul rax, COLS
    add byte [result + rax], 3
    inc rcx
    jmp .p2_left_loop

.p2_right_col:
    mov rcx, 1
.p2_right_loop:
    cmp rcx, ROWS-1
    jge .p2_corners
    mov rax, rcx
    imul rax, COLS
    add rax, COLS-1
    add byte [result + rax], 3
    inc rcx
    jmp .p2_right_loop

.p2_corners:
    add byte [result], 2
    add byte [result + COLS - 1], 2
    add byte [result + (ROWS-1) * COLS], 2
    add byte [result + (ROWS-1) * COLS + COLS - 1], 2

    xor rcx, rcx
.p2_filter_loop:
    cmp rcx, MATRIX_SIZE
    jge .p2_count_and_activate

    cmp byte [my_matrix + rcx], 1
    jne .p2_filter_next
    mov byte [result + rcx], 0

.p2_filter_next:
    inc rcx
    jmp .p2_filter_loop

.p2_count_and_activate:
    xor rbx, rbx            ; rbx = new_activations this iteration
    xor rcx, rcx

.p2_count_loop:
    cmp rcx, MATRIX_SIZE
    jge .p2_check_continue

    cmp byte [result + rcx], 5
    jl .p2_count_next

    inc rbx                         ; new_activations++
    mov byte [my_matrix + rcx], 1   ; activate: becomes paper roll

.p2_count_next:
    inc rcx
    jmp .p2_count_loop

.p2_check_continue:
    ; counter_p2 += new_activations
    add [counter_p2], rbx

    ; if new_activations > 0, continue propagating
    test rbx, rbx
    jnz .p2_main_loop

.print_results:
    ; Print "Counter P1: "
    mov rax, 1
    mov rdi, 1
    mov rsi, msg_p1
    mov rdx, msg_p1_len
    syscall

    ; Convert counter_p1 to string
    mov rax, [counter_p1]
    call .print_number_in_rax

    ; Print "Counter P2: "
    mov rax, 1
    mov rdi, 1
    mov rsi, msg_p2
    mov rdx, msg_p2_len
    syscall

    ; Convert counter_p2 to string
    mov rax, [counter_p2]
    call .print_number_in_rax

    jmp .exit

.print_number_in_rax:
    lea rdi, [buffer + 15]
    mov rcx, 10

    test rax, rax
    jnz .convert_loop
    ; Handle zero case
    dec rdi
    mov byte [rdi], '0'
    jmp .do_print

.convert_loop:
    test rax, rax
    jz .do_print
    xor rdx, rdx
    div rcx                 ; rax = quotient, rdx = remainder
    add dl, '0'
    dec rdi
    mov [rdi], dl
    jmp .convert_loop

.do_print:
    ; Calculate length and print
    lea rdx, [buffer + 15]
    sub rdx, rdi            ; rdx = length
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
    ret

.exit:
    ;; 下次見～
    mov rax, 60
    xor rdi, rdi
    syscall
