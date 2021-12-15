	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
	pushq $0
	movq $16, %rdi
	call allocz
	movq %rax, %rdi
	movq %rdi, -8(%rbp)
	movq -8(%rbp), %rdi
	addq $0, %rdi
	movq %rdi, %rax
	movq $12, %rdi
	movq %rdi, 0(%rax)
	pushq $0
	movq $16, %rdi
	call allocz
	movq %rax, %rdi
	movq %rdi, -16(%rbp)
	movq -16(%rbp), %rdi
	addq $0, %rdi
	movq %rdi, %rax
	movq $2, %rdi
	movq %rdi, 0(%rax)
	movq -16(%rbp), %rdi
	addq $8, %rdi
	movq %rdi, %rax
	movq -8(%rbp), %rdi
	movq %rdi, 0(%rax)
#begin print
	movq -16(%rbp), %rdi
	movq 0(%rdi), %rdi
	movq $S_8, %rdi
	call print_string
	call print_space
	movq -16(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
	movq $S_7, %rdi
	call print_string
	call print_space
	movq $S_6, %rdi
	call print_string
	call print_space
	movq -16(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	call print_space
	movq $S_5, %rdi
	call print_string
	call print_space
	movq -8(%rbp), %rdi
	movq 0(%rdi), %rdi
	movq $S_4, %rdi
	call print_string
	call print_space
	movq -8(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
	movq $S_3, %rdi
	call print_string
	call print_space
	movq $S_2, %rdi
	call print_string
	call print_space
	movq -8(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	call print_space
	movq $S_1, %rdi
	call print_string
#end print
	addq $16, %rsp
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
      mov     $S_false, %rdi
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
S_2:
	.string "next :"
S_6:
	.string "next :"
S_4:
	.string "x :"
S_5:
	.string "\n"
S_7:
	.string "\n"
S_8:
	.string "x :"
S_3:
	.string "\n"
S_1:
	.string "\n"
