	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_fact:
	pushq %rbp
	movq %rsp, %rbp
	pushq $0
	movq $1, %rdi
	movq %rdi, -8(%rbp)
L_1:
	movq 16(%rbp), %rdi
	movq %rdi, %rsi
	movq $1, %rdi
	cmpq %rdi, %rsi
	jg L_3
	movq $0, %rdi
	jmp L_4
L_3:
	movq $1, %rdi
L_4:
	testq %rdi, %rdi
	jz L_2
	movq -8(%rbp), %rdi
	movq %rdi, %rsi
	movq 16(%rbp), %rdi
	imulq %rsi, %rdi
	movq %rdi, -8(%rbp)
	movq %rbp, %rdi
	addq $16, %rdi
	subq $1, 0(%rdi)
	jmp L_1
L_2:
	movq -8(%rbp), %rdi
	movq %rdi, %rax
	call E_fact
	addq $8, %rsp
E_fact:
	movq %rbp, %rsp
	popq %rbp
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
	pushq $0
	movq $0, %rdi
	movq %rdi, -8(%rbp)
L_5:
	movq -8(%rbp), %rdi
	movq %rdi, %rsi
	movq $10, %rdi
	cmpq %rdi, %rsi
	jle L_7
	movq $0, %rdi
	jmp L_8
L_7:
	movq $1, %rdi
L_8:
	testq %rdi, %rdi
	jz L_6
	movq -8(%rbp), %rdi
	pushq %rdi
	call F_fact
	movq %rax, %rdi
	addq $8, %rsp
	call print_int
	call print_space
	movq $S_1, %rdi
	call print_string
	call print_space
	movq %rbp, %rdi
	addq $-8, %rdi
	addq $1, 0(%rdi)
	jmp L_5
L_6:
	addq $8, %rsp
E_main:
	movq %rbp, %rsp
	popq %rbp
	ret

print_int_or_nil:
      test    %rdi, %rdi
      jz      print_nil
      movq    (%rdi), %rdi
print_int:
      movq    %rdi, %rsi
      movq    $S_int, %rdi
      xorq    %rax, %rax
      call    printf
      ret
print_string:
      test    %rdi, %rdi
      jz      print_nil
      mov     %rdi, %rsi
      mov     $S_string, %rdi
      xorq    %rax, %rax
      call    printf
      ret
print_nil:
      mov     $S_nil, %rdi
      xorq    %rax, %rax
      call    printf
      ret
print_space:
      mov     $S_space, %rdi
      xorq    %rax, %rax
      call    printf
      ret
print_bool:
      xorq    %rax, %rax
      test    %rdi, %rdi
      jz      1f
      mov     $S_true, %rdi
      call    printf
      ret
1:      mov     $S_false, %rdi
      call    printf
      ret
allocz:
      movq    %rdi, %rbx     # callee-save
        call    malloc
        testq   %rbx, %rbx
        jnz     1f
        ret
1:      movb    $0, (%rax, %rbx)
        decq    %rbx
        jnz     1b
        ret
	.data
S_int:
	.string "%ld"
S_string:
	.string "%s"
S_true:
	.string "true"
S_false:
	.string "false"
S_nil:
	.string "<nil>"
S_space:
	.string " "
S_empty:
	.string ""
S_1:
	.string "\n"
