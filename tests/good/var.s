	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
	subq %rsp, $8
	movq $2, %rdi
	movq -4(%rbp), %rdi
	popq %rdi
	call print_int
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
1:    mov     $S_false, %rdi
      call    printf
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
