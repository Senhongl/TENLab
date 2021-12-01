	.text
	.file	"TensorC"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	.Lstensor(%rip), %rdi
	leaq	.Lstensor.3(%rip), %rsi
	callq	subtract@PLT
	movq	%rax, %rdi
	callq	print@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lsdim,@object          # @sdim
	.section	.rodata,"a",@progbits
.Lsdim:
	.ascii	"\002\003"
	.size	.Lsdim, 2

	.type	.Lsdata,@object         # @sdata
	.section	.rodata.str4.16,"aMS",@progbits,4
	.p2align	4
.Lsdata:
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	1                       # 0x1
	.long	0                       # 0x0
	.size	.Lsdata, 24

	.type	.Lstensor,@object       # @stensor
	.section	.data.rel.ro,"aw",@progbits
	.p2align	4
.Lstensor:
	.byte	0                       # 0x0
	.byte	2                       # 0x2
	.zero	6
	.quad	.Lsdim
	.quad	.Lsdata
	.size	.Lstensor, 24

	.type	.Lsdim.1,@object        # @sdim.1
	.section	.rodata,"a",@progbits
.Lsdim.1:
	.ascii	"\002\003"
	.size	.Lsdim.1, 2

	.type	.Lsdata.2,@object       # @sdata.2
	.p2align	4
.Lsdata.2:
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	5                       # 0x5
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	0                       # 0x0
	.size	.Lsdata.2, 24

	.type	.Lstensor.3,@object     # @stensor.3
	.section	.data.rel.ro,"aw",@progbits
	.p2align	4
.Lstensor.3:
	.byte	0                       # 0x0
	.byte	2                       # 0x2
	.zero	6
	.quad	.Lsdim.1
	.quad	.Lsdata.2
	.size	.Lstensor.3, 24

	.section	".note.GNU-stack","",@progbits
