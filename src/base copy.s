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
	movq $1, %rdi
	movq %rdi, %r12
#Uamp of aux1
	movq %rbp, %rdi
	addq $-8, %rdi
	movq 0(%rdi), %rdi
	addq $0, %rdi
	movq %r12, 0(%rdi)
	movq $2, %rdi
	movq %rdi, %r12
#Uamp of aux1
	movq %rbp, %rdi
	addq $-8, %rdi
	movq 0(%rdi), %rdi
	addq $8, %rdi
	movq %r12, 0(%rdi)
#begin print
#ident of aux1
	movq -8(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
#ident of aux1
	movq -8(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	call print_space
	movq $S_2, %rdi
	call print_string
	call print_space
#end print
	pushq $0
	movq $16, %rdi
	call allocz
	movq %rax, %rdi
	movq %rdi, -16(%rbp)
#ident of aux1
	movq -8(%rbp), %rdi
	movq 0(%rdi), %rdi
	movq %rdi, %rsi
	movq $2, %rdi
	addq %rsi, %rdi
	movq %rdi, %r12
#Uamp of aux2
	movq %rbp, %rdi
	addq $-16, %rdi
	movq 0(%rdi), %rdi
	addq $0, %rdi
	movq %r12, 0(%rdi)
#ident of aux1
	movq -8(%rbp), %rdi
	movq 8(%rdi), %rdi
	movq %rdi, %rsi
	movq $2, %rdi
	addq %rsi, %rdi
	movq %rdi, %r12
#Uamp of aux2
	movq %rbp, %rdi
	addq $-16, %rdi
	movq 0(%rdi), %rdi
	addq $8, %rdi
	movq %r12, 0(%rdi)
#begin print
#ident of aux2
	movq -16(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
#ident of aux2
	movq -16(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	call print_space
	movq $S_1, %rdi
	call print_string
	call print_space
#end print
	pushq $0
	movq $16, %rdi
	call allocz
	movq %rax, %rdi
	movq %rdi, -24(%rbp)
#ident of aux2
	movq -16(%rbp), %rdi
	movq 0(%rdi), %rdi
	movq %rdi, %r12
#Uamp of aux3
	movq %rbp, %rdi
	addq $-24, %rdi
	movq 0(%rdi), %rdi
	addq $0, %rdi
	movq %r12, 0(%rdi)
#ident of aux2
	movq -16(%rbp), %rdi
	movq 8(%rdi), %rdi
	movq %rdi, %r12
#Uamp of aux3
	movq %rbp, %rdi
	addq $-24, %rdi
	movq 0(%rdi), %rdi
	addq $8, %rdi
	movq %r12, 0(%rdi)
#ident of aux3
	movq -24(%rbp), %rdi
	movq 0(%rdi), %rdi
	movq %rdi, %rsi
	movq $2, %rdi
	addq %rsi, %rdi
	movq %rdi, %r12
#Uamp of aux3
	movq %rbp, %rdi
	addq $-24, %rdi
	movq 0(%rdi), %rdi
	addq $0, %rdi
	movq %r12, 0(%rdi)
#ident of aux3
	movq -24(%rbp), %rdi
	movq 8(%rdi), %rdi
	movq %rdi, %rsi
	movq $2, %rdi
	addq %rsi, %rdi
	movq %rdi, %r12
#Uamp of aux3
	movq %rbp, %rdi
	addq $-24, %rdi
	movq 0(%rdi), %rdi
	addq $8, %rdi
	movq %r12, 0(%rdi)
#begin print
#ident of aux3
	movq -24(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
#ident of aux3
	movq -24(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	call print_space
#end print
	pushq $0
	movq $16, %rdi
	call allocz
	movq %rax, %rdi
	movq %rdi, -32(%rbp)
#ident of aux3
	movq -24(%rbp), %rdi
	movq 0(%rdi), %rdi
	movq %rdi, -32(%rbp)
#begin print
#ident of d
	movq -32(%rbp), %rdi
	movq 0(%rdi), %rdi
	call print_int
	call print_space
#ident of d
	movq -32(%rbp), %rdi
	movq 8(%rdi), %rdi
	call print_int
	call print_space
#end print
	addq $32, %rsp
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
S_2:
	.string "\n"
S_1:
	.string "\n"
