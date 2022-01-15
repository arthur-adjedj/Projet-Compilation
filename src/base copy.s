	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
#start block
#start block
	pushq $0
#start assign of ident : x
	movq $10, %rdi
	movq %rdi, -8(%rbp)
#end assign of ident : x
	pushq $0
#start assign of ident : y
#Uamp of x
	movq %rbp, %rdi
	addq $-8, %rdi
	movq %rdi, -16(%rbp)
#end assign of ident : y
#start assign of ident : y
#ident of y
	movq -16(%rbp), %rdi
	movq %rdi, -16(%rbp)
#end assign of ident : y
#begin print
#ident of x
	movq -8(%rbp), %rdi
	call print_int
	call print_space
	movq $S_1, %rdi
	call print_string
#end print
#end block
#end block
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
S_1:
	.string "\n"
