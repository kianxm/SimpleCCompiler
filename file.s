main:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$main.size, %eax
	subq	%rax, %rsp
	movl	$100,	%eax
	movl	%eax, -12(%rbp)
	movl	$30,	%eax
	movb	%al, -30(%rbp)
	movl	$2,	%eax
	movb	%al, -31(%rbp)
	movsbl	-30(%rbp),	%eax
	movl	-12(%rbp),	%edi
	addl	%eax,	%edi
	movsbl	-31(%rbp),	%eax
	addl	%eax,	%edi
	movl	%edi, -4(%rbp)
	movsbl	-30(%rbp),	%eax
	movl	-12(%rbp),	%edi
	subl	%eax,	%edi
	movsbl	-31(%rbp),	%eax
	subl	%eax,	%edi
	movslq	%edi,	%rdi
	movq	%rdi, -20(%rbp)
	movsbl	-30(%rbp),	%eax
	movl	-12(%rbp),	%edi
	imull	%eax,	%edi
	movsbl	-31(%rbp),	%eax
	imull	%eax,	%edi
	movslq	%edi,	%rdi
	movq	%rdi, -28(%rbp)
	movsbl	-30(%rbp),	%eax
	movl	%eax, -35(%rbp)
	movl	-12(%rbp),	%eax
	movl	-35(%rbp),	%ecx
	cltd	
	idivl	%ecx
	movsbl	-31(%rbp),	%edi
	addl	%edi,	%eax
	movl	%eax, -8(%rbp)
	movsbl	-30(%rbp),	%eax
	movl	%eax, -39(%rbp)
	movl	-12(%rbp),	%eax
	movl	-39(%rbp),	%ecx
	cltd	
	idivl	%ecx
	movsbl	-31(%rbp),	%eax
	subl	%eax,	%edx
	movb	%dl, -29(%rbp)
	leaq	.L0,	%rax
	movl	-4(%rbp),	%esi
	movq	%rax,	%rdi
	movl	$0, %eax
	call	printf
	leaq	.L1,	%rax
	movq	-20(%rbp),	%rsi
	movq	%rax,	%rdi
	movl	$0, %eax
	call	printf
	leaq	.L1,	%rax
	movq	-28(%rbp),	%rsi
	movq	%rax,	%rdi
	movl	$0, %eax
	call	printf
	leaq	.L0,	%rax
	movl	-8(%rbp),	%esi
	movq	%rax,	%rdi
	movl	$0, %eax
	call	printf
	movsbl	-29(%rbp),	%eax
	leaq	.L0,	%rdi
	movl	%eax,	%esi
	movl	$0, %eax
	call	printf

main.exit:
	movq	%rbp, %rsp
	popq	%rbp
	ret

	.set	main.size, 48
	.globl	main

	.data
.L0:	.asciz	"%d\012"
.L1:	.asciz	"%ld\012"
