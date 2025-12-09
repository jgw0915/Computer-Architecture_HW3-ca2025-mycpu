	.section .text.init
.globl _start
_start:
  # Initialize global pointer for linker relaxation
  .option push
  .option norelax
  la gp, __global_pointer$
  .option pop

  # Initialize stack pointer (4MB - standard)
  li sp, 0x00400000

  # Clear .sbss section (small uninitialized data)
  la t0, __sbss_start
  la t1, __sbss_end
sbss_clear_loop:
  bgeu t0, t1, sbss_clear_done
  sw zero, 0(t0)
  addi t0, t0, 4
  j sbss_clear_loop
sbss_clear_done:

  # Clear .bss section (large uninitialized data)
  la t0, __bss_start
  la t1, __bss_end
bss_clear_loop:
  bgeu t0, t1, bss_clear_done
  sw zero, 0(t0)
  addi t0, t0, 4
  j bss_clear_loop
bss_clear_done:

  # Call main function
  call main
  
  # Loop forever (CPU halt)
loop:
  wfi
  j loop

	.align	2
	.globl	get_cycles
	.type	get_cycles, @function
get_cycles:
	addi	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	addi	s0,sp,32
.L2:
 #APP
# 31 "main_revisit.c" 1
	csrr t1, cycleh
# 0 "" 2
 #NO_APP
	sw	t1,-20(s0)
 #APP
# 32 "main_revisit.c" 1
	csrr t1, cycle
# 0 "" 2
 #NO_APP
	sw	t1,-24(s0)
 #APP
# 33 "main_revisit.c" 1
	csrr t1, cycleh
# 0 "" 2
 #NO_APP
	sw	t1,-28(s0)
	lw	t3,-20(s0)
	lw	t1,-28(s0)
	bne	t3,t1,.L2
	lw	t1,-20(s0)
	mv	a6,t1
	li	a7,0
	slli	a5,a6,0
	li	a4,0
	lw	a6,-24(s0)
	mv	a2,a6
	li	a3,0
	or	a0,a4,a2
	or	a1,a5,a3
	mv	a4,a0
	mv	a5,a1
	mv	a0,a4
	mv	a1,a5
	lw	ra,28(sp)
	lw	s0,24(sp)
	addi	sp,sp,32
	jr	ra
	.size	get_cycles, .-get_cycles
	.align	2
	.globl	memcpy
	.type	memcpy, @function
memcpy:
	addi	sp,sp,-48
	sw	ra,44(sp)
	sw	s0,40(sp)
	addi	s0,sp,48
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	sw	a2,-44(s0)
	lw	a5,-36(s0)
	sw	a5,-20(s0)
	lw	a5,-40(s0)
	sw	a5,-24(s0)
	j	.L5
.L6:
	lw	a4,-24(s0)
	addi	a5,a4,1
	sw	a5,-24(s0)
	lw	a5,-20(s0)
	addi	a3,a5,1
	sw	a3,-20(s0)
	lbu	a4,0(a4)
	sb	a4,0(a5)
.L5:
	lw	a5,-44(s0)
	addi	a4,a5,-1
	sw	a4,-44(s0)
	bne	a5,zero,.L6
	lw	a5,-36(s0)
	mv	a0,a5
	lw	ra,44(sp)
	lw	s0,40(sp)
	addi	sp,sp,48
	jr	ra
	.size	memcpy, .-memcpy
	.align	2
	.globl	mul32
	.type	mul32, @function
mul32:
	addi	sp,sp,-48
	sw	ra,44(sp)
	sw	s0,40(sp)
	addi	s0,sp,48
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	li	a0,0
	li	a1,0
	sw	a0,-24(s0)
	sw	a1,-20(s0)
	sw	zero,-28(s0)
	j	.L9
.L13:
	lw	a1,-28(s0)
	li	a0,1
	sll	a0,a0,a1
	lw	a1,-40(s0)
	and	a1,a0,a1
	beq	a1,zero,.L10
	lw	a1,-36(s0)
	mv	a2,a1
	li	a3,0
	lw	a1,-28(s0)
	addi	a1,a1,-32
	blt	a1,zero,.L11
	sll	a5,a2,a1
	li	a4,0
	j	.L12
.L11:
	srli	a0,a2,1
	li	a6,31
	lw	a1,-28(s0)
	sub	a1,a6,a1
	srl	a1,a0,a1
	lw	a0,-28(s0)
	sll	a5,a3,a0
	or	a5,a1,a5
	lw	a1,-28(s0)
	sll	a4,a2,a1
.L12:
	lw	a6,-24(s0)
	lw	a7,-20(s0)
	add	a0,a6,a4
	mv	t1,a0
	sltu	t1,t1,a6
	add	a1,a7,a5
	add	a6,t1,a1
	mv	a1,a6
	sw	a0,-24(s0)
	sw	a1,-20(s0)
.L10:
	lw	a1,-28(s0)
	addi	a1,a1,1
	sw	a1,-28(s0)
.L9:
	lw	a0,-28(s0)
	li	a1,31
	ble	a0,a1,.L13
	lw	a4,-24(s0)
	lw	a5,-20(s0)
	mv	a0,a4
	mv	a1,a5
	lw	ra,44(sp)
	lw	s0,40(sp)
	addi	sp,sp,48
	jr	ra
	.size	mul32, .-mul32
	.align	2
	.globl	clz
	.type	clz, @function
clz:
	addi	sp,sp,-48
	sw	ra,44(sp)
	sw	s0,40(sp)
	addi	s0,sp,48
	sw	a0,-36(s0)
	lw	a5,-36(s0)
	bne	a5,zero,.L16
	li	a5,32
	j	.L17
.L16:
	sw	zero,-20(s0)
	lw	a4,-36(s0)
	li	a5,65536
	bgeu	a4,a5,.L18
	lw	a5,-20(s0)
	addi	a5,a5,16
	sw	a5,-20(s0)
	lw	a5,-36(s0)
	slli	a5,a5,16
	sw	a5,-36(s0)
.L18:
	lw	a4,-36(s0)
	li	a5,16777216
	bgeu	a4,a5,.L19
	lw	a5,-20(s0)
	addi	a5,a5,8
	sw	a5,-20(s0)
	lw	a5,-36(s0)
	slli	a5,a5,8
	sw	a5,-36(s0)
.L19:
	lw	a4,-36(s0)
	li	a5,268435456
	bgeu	a4,a5,.L20
	lw	a5,-20(s0)
	addi	a5,a5,4
	sw	a5,-20(s0)
	lw	a5,-36(s0)
	slli	a5,a5,4
	sw	a5,-36(s0)
.L20:
	lw	a4,-36(s0)
	li	a5,1073741824
	bgeu	a4,a5,.L21
	lw	a5,-20(s0)
	addi	a5,a5,2
	sw	a5,-20(s0)
	lw	a5,-36(s0)
	slli	a5,a5,2
	sw	a5,-36(s0)
.L21:
	lw	a5,-36(s0)
	blt	a5,zero,.L22
	lw	a5,-20(s0)
	addi	a5,a5,1
	sw	a5,-20(s0)
.L22:
	lw	a5,-20(s0)
.L17:
	mv	a0,a5
	lw	ra,44(sp)
	lw	s0,40(sp)
	addi	sp,sp,48
	jr	ra
	.size	clz, .-clz
	.section	.rodata
	.align	2
	.type	rsqrt_table, @object
	.size	rsqrt_table, 128
rsqrt_table:
	.word	65536
	.word	46341
	.word	32768
	.word	23170
	.word	16384
	.word	11585
	.word	8192
	.word	5793
	.word	4096
	.word	2896
	.word	2048
	.word	1448
	.word	1024
	.word	724
	.word	512
	.word	362
	.word	256
	.word	181
	.word	128
	.word	90
	.word	64
	.word	45
	.word	32
	.word	23
	.word	16
	.word	11
	.word	8
	.word	6
	.word	4
	.word	3
	.word	2
	.word	1
	.text
	.align	2
	.globl	fast_rsqrt
	.type	fast_rsqrt, @function
fast_rsqrt:
	 # prologue
    addi    sp,sp,-32
    sw      s0,24(sp)
    sw      ra,28(sp)
    li      s0,-1                  # C edge: return value for x==0
    beq     a0,zero,ret_zero_case  # if (x==0)

    sw      s1,20(sp)
    sw      s2,16(sp)
    li      s2,1
    mv      s1,a0                  # s1 = x
    li      s0,65536               # C edge: return value for x==1
    beq     a0,s2,ret_one_case     # if (x==1)

    sw      s3,12(sp)
    sw      s4,8(sp)

    # C03: exp = 31 - clz(x)
    call    clz
    li      a5,31
    sub     a5,a5,a0               # a5 = exp

    # C04: y = rsqrt_table[exp]
    lui     a4,%hi(rsqrt_table)
    slli    a2,a5,2
    addi    a4,a4,%lo(rsqrt_table)
    add     a2,a4,a2
    sll     s2,s2,a5               # s2 = (1u << exp)
    lw      s0,0(a2)               # s0 = y

    mv      a3,a0                  # keep clz(x) in a3 (as in original)
    bltu    s2,s1,interp_entry     # if (x > 1<<exp) → interpolate

newton_two_iters:
    li      s3,196608              # (3<<16)

    # ---- iteration 1 ----
    mv      a1,s0
    mv      a0,s0
    call    mul32                  # y2
    mv      a1,a0
    mv      a0,s1
    call    mul32                  # x*y2
    slli    a1,a1,16
    srli    a0,a0,16
    or      a0,a1,a0               # xy2 (Q16)
    sub     a1,s3,a0               # (3<<16)-xy2
    mv      a0,s0
    call    mul32
    srli    a0,a0,17
    slli    a1,a1,15
    or      s0,a1,a0               # y updated

    # ---- iteration 2 ----
    mv      a1,s0
    mv      a0,s0
    call    mul32                  # y2
    mv      a1,a0
    mv      a0,s1
    call    mul32                  # x*y2
    slli    a1,a1,16
    srli    a0,a0,16
    or      a0,a1,a0               # xy2 (Q16)
    sub     a1,s3,a0               # (3<<16)-xy2
    mv      a0,s0
    call    mul32
    srli    a0,a0,17
    slli    a1,a1,15
    or      s0,a1,a0               # y updated

    # epilogue for common path
    lw      s1,20(sp)
    lw      s2,16(sp)
    lw      s3,12(sp)
    lw      s4,8(sp)
    lw      ra,28(sp)
    mv      a0,s0
    lw      s0,24(sp)
    addi    sp,sp,32
    jr      ra

ret_zero_case:
    lw      ra,28(sp)
    mv      a0,s0
    lw      s0,24(sp)
    addi    sp,sp,32
    jr      ra

interp_entry:
    # C05–C08: linear interpolation toward next table entry when x > 2^exp
    li      a2,30
    mv      a0,s0
    bgt     a5,a2,interp_have_delta    # if exp>30, next=y_next=0 → delta=y

    # a0 = delta = y - y_next with safe table bound
    li      a2,32
    sub     a3,a2,a3
    slli    a3,a3,2
    add     a4,a4,a3
    lw      a0,0(a4)
    sub     a0,s0,a0

interp_have_delta:
    addi    a3,a5,-32
    blt     a3,zero,interp_frac_le_shift_prep

    # build frac = ((x - 2^exp)<<16)>>exp  in 32-bit, several packs
    li      a1,1
    sll     a1,a1,a3
    li      a4,0

interp_frac_build:
    sub     a4,s1,a4               # running difference
    sgtu    a2,a4,s1
    neg     a1,a1
    sub     a1,a1,a2
    slli    a1,a1,16
    srli    a2,a4,16
    or      a1,a2,a1
    slli    a4,a4,16
    blt     a3,zero,interp_frac_crosspack
    srl     a1,a1,a3

interp_apply_delta:
    call    mul32                  # mul32(delta, frac)
    srli    a0,a0,16               # >>16
    slli    a1,a1,16
    or      a0,a1,a0
    sub     s0,s0,a0               # y -= delta*frac>>16
    j       newton_two_iters

interp_frac_le_shift_prep:
    li      a4,1
    sll     a4,a4,a5
    li      a1,0
    j       interp_frac_build

ret_one_case:
    lw      ra,28(sp)
    mv      a0,s0
    lw      s0,24(sp)
    lw      s1,20(sp)
    lw      s2,16(sp)
    addi    sp,sp,32
    jr      ra

interp_frac_crosspack:
    li      a2,31
    slli    a3,a1,1
    sub     a2,a2,a5
    srl     a1,a4,a5
    sll     a5,a3,a2
    or      a1,a5,a1
    j       interp_apply_delta
	.size	fast_rsqrt, .-fast_rsqrt
	.section	.rodata
	.align	2
.LC0:
	.word	1
	.word	4
	.word	16
	.word	20
	.word	30
	.word	100
	.word	120
	.word	130
	.word	0
	.word	-1
	.text
	.align	2
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-112
	sw	ra,108(sp)
	sw	s0,104(sp)
	sw	s2,100(sp)
	sw	s3,96(sp)
	addi	s0,sp,112
	lui	a5,%hi(.LC0)
	addi	a5,a5,%lo(.LC0)
	lw	t3,0(a5)
	lw	t1,4(a5)
	lw	a7,8(a5)
	lw	a6,12(a5)
	lw	a0,16(a5)
	lw	a1,20(a5)
	lw	a2,24(a5)
	lw	a3,28(a5)
	lw	a4,32(a5)
	lw	a5,36(a5)
	sw	t3,-108(s0)
	sw	t1,-104(s0)
	sw	a7,-100(s0)
	sw	a6,-96(s0)
	sw	a0,-92(s0)
	sw	a1,-88(s0)
	sw	a2,-84(s0)
	sw	a3,-80(s0)
	sw	a4,-76(s0)
	sw	a5,-72(s0)
	li	a5,4
	sw	a5,-20(s0)
	li	a5,8
	sw	a5,-24(s0)
	sw	zero,-28(s0)
	j	.L37
.L38:
	lw	a4,-28(s0)
	addi	a5,s0,-108
	slli	a4,a4,2
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-32(s0)
	call	get_cycles
	sw	a0,-40(s0)
	sw	a1,-36(s0)
	lw	a0,-32(s0)
	call	fast_rsqrt
	sw	a0,-44(s0)
	lw	a5,-20(s0)
	lw	a4,-44(s0)
	sw	a4,0(a5)
	call	get_cycles
	sw	a0,-56(s0)
	sw	a1,-52(s0)
	lw	a2,-56(s0)
	lw	a3,-52(s0)
	lw	a0,-40(s0)
	lw	a1,-36(s0)
	sub	a4,a2,a0
	mv	a6,a4
	sgtu	a6,a6,a2
	sub	a5,a3,a1
	sub	a3,a5,a6
	mv	a5,a3
	sw	a4,-64(s0)
	sw	a5,-60(s0)
	lw	a5,-24(s0)
	sw	a5,-68(s0)
	lw	a4,-64(s0)
	lw	a5,-68(s0)
	sw	a4,0(a5)
	lw	a5,-60(s0)
	srli	s2,a5,0
	li	s3,0
	lw	a5,-68(s0)
	addi	a5,a5,4
	mv	a4,s2
	sw	a4,0(a5)
	lw	a5,-20(s0)
	addi	a5,a5,12
	sw	a5,-20(s0)
	lw	a5,-24(s0)
	addi	a5,a5,12
	sw	a5,-24(s0)
	lw	a5,-28(s0)
	addi	a5,a5,1
	sw	a5,-28(s0)
.L37:
	lw	a4,-28(s0)
	li	a5,9
	bleu	a4,a5,.L38
	li	a5,0
	mv	a0,a5
	lw	ra,108(sp)
	lw	s0,104(sp)
	lw	s2,100(sp)
	lw	s3,96(sp)
	addi	sp,sp,112
	jr	ra
	.size	main, .-main