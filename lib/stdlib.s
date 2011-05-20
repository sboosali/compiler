.data
	outofbounds: .asciiz "Aborting: Out-of-bounds error\n"
	nullpointerdereference: .asciiz "Aborting: Null-pointer dereference\n"
	nullpointerassignment: .asciiz "Aborting: Null-pointer assignment\n"
	stdabort: .asciiz "Aborting\n"
	illegalcode: .asciiz "Aborting (WARNING: Illegal abort code supplied)\n"

.text
	
.globl main

# ***************************************************************************************************************
# Subroutine: a_getchar
# Read a character from standard input; return empty string on end of file
#
# Params: <none>
# Returns: $v0 - a pointer to the string representing the character read in
# 
# Register Usage:
# 
#
# ***************************************************************************************************************

a_getchar:
	sub $sp, $sp, 8
	sw $ra, 4($sp)
	li $a0, 0
	la $a1, 8($sp)
	li $a2, 1	
	sub $sp, $sp, 16
	jal read
	add $sp, $sp, 16	
	beq $v0, 0, getchar_eof
	beq $v0, -1, getchar_interrupt
	li $a0, 6
	sub $sp, $sp, 16
	jal malloc
	add $sp, $sp, 16
	li $t0, 1
	sw $t0, ($v0)
	lbu $t0, 8($sp)
	sb $t0, 4($v0)	
	li $t0, 0
	sb $t0, 5($v0)
	j getchar_interrupt	
	getchar_eof:
		li $a0, 5
		sub $sp, $sp, 16
		jal malloc
		add $sp, $sp, 16
		li $t0, 0
		sw $t0, ($v0)
		sw $t0, 4($v0)
		j getchar_interrupt
	end_getchar_eof:
	getchar_interrupt:
	lw $ra, 4($sp)
	add $sp, $sp, 8
	jr $ra

# ***************************************************************************************************************
# Subroutine: a_ord
# Returns the ASCII value of the first character of s; yields -1 if s is the empty string
#
# Params: $a0 - a pointer to the string whose first character we will returns
# Returns: $v0 - a pointer to the string representing the character read in
# 
# Register Usage:
# 
#
# ***************************************************************************************************************

a_ord:
	sub $sp, $sp, 4
	sw $ra, 4($sp)
	lw $t0, ($a0)
	beq $t0, 0, empty_case
	lbu $v0, 4($a0)
	j end_empty_case
	empty_case:	
		li $v0, -1
	end_empty_case:
	lw $ra, 4($sp)
	add $sp, $sp, 4
	jr $ra

# ***************************************************************************************************************
# Subroutine: a_print
# Prints a string s on standard output
#
# Params: $a0 - a pointer to the string that will be printed
# Returns: $v0 - a pointer to the string representing the character read in
# 
# Register Usage:
# 
#
# ***************************************************************************************************************

a_print:
	sub $sp, $sp, 4
	sw $ra, 4($sp)
	move $t0, $a0	
	li $a0, 1
	la $a1, 4($t0)
	lw $a2, ($t0)
	sub $sp, $sp, 16
	jal write
	add $sp, $sp, 16
	lw $ra, 4($sp)
	add $sp, $sp, 4
	jr $ra

a_size:
	lw $v0, ($a0)
	jr $ra
	
a_not:
	seq $v0, $a0, 0
	jr $ra
	
a_chr: #fix to exit if i is out of range
	sub $sp, $sp, 8
	sw $ra, 4($sp)
	sw $s0, 8($sp)
	bgt $a0, 127, out_of_range
	blt $a0, 0, out_of_range
	j in_range
	out_of_range:
		li $a0, 1
		jal a_error
	in_range:
	move $s0, $a0
	li $a0, 6
	sub $sp, $sp, 16
	jal malloc
	add $sp, $sp, 16
	li $t0, 1
	sw $t0, ($v0)
	sb $s0, 4($v0)
	li $t0, 0
	sb $t0, 5($v0)
	lw $s0, 8($sp)
	lw $ra, 4($sp)
	add $sp, $sp,8
	jr $ra

a_exit:
	sub $sp, $sp, 16
	jal exit
	
a_error:
	beq $a0, 0, ab_stdabort
	beq $a0, 1, ab_outofbounds
	beq $a0, 2, ab_nullpointerdereference
	beq $a0, 3, ab_nullpointerassignment
	j ab_illegalcode
	ab_stdabort:
		li $a0, 1	
		la $a1, stdabort
		li $a2, 9
		sub $sp, $sp, 16
		jal write
		add $sp, $sp, 16
		jal a_abort
	ab_outofbounds:
		li $a0, 1	
		la $a1, outofbounds
		li $a2, 30
		sub $sp, $sp, 16
		jal write
		add $sp, $sp, 16
		jal a_abort
	ab_nullpointerdereference:
		li $a0, 1	
		la $a1, nullpointerdereference
		li $a2, 35
		sub $sp, $sp, 16
		jal write
		add $sp, $sp, 16
		jal a_abort
	ab_nullpointerassignment:
		li $a0, 1	
		la $a1, nullpointerassignment
		li $a2, 34
		sub $sp, $sp, 16
		jal write
		add $sp, $sp, 16
		jal a_abort
	ab_illegalcode:
		li $a0, 1	
		la $a1, illegalcode
		li $a2, 48
		sub $sp, $sp, 16
		jal write
		add $sp, $sp, 16
		jal a_abort

a_abort:
	sub $sp, $sp, 16
	jal abort
	
a_flush:
	jr $ra
	
a_itoa:
	sub $sp, $sp, 16
	sw $ra, 4($sp)
	sw $s0, 8($sp)
	sw $s1, 12($sp)
	sw $s2, 16($sp)
	li $s0, 1
	move $s1, $a0 #s1 holds the number we will convert
	while_size_check:
		div $a0, $a0, 10
		beq $a0, 0, end_while_size_check
		add $s0, $s0, 1
		j while_size_check
	end_while_size_check: # $s0 now holds the length of the "string", number of digits we have
	move $a0, $s0
	add $a0, $a0, 5 #word size + null termination character
	move $s2, $s1
	blt $s1, 0, lt_zero
	ge_zero:
		sub $sp, $sp, 16
		jal malloc
		add $sp, $sp, 16
		sw $s0, ($v0)
		add $t0, $v0, 4
		j end_lt_zero
	end_ge_zero:
	lt_zero:
		add $s0, $s0, 1 #the length of the string is now +1, since we have the negative sign "-"
		add $a0, $a0, 1 #we need to add the negative sign if it's less than zero
		sub $sp, $sp, 16
		jal malloc
		add $sp, $sp, 16
		sw $s0, ($v0)
		sub $s0, $s0, 1
		li $t0, 45 #ascii value of the negative sign
		sb $t0, 4($v0)
		la $t0, 5($v0)	
	end_lt_zero:
	add $t3, $t0, $s0
	sb $0, ($t3) #store the null-termination character first
	sub $s0, $s0, 1
	while_string_make: # $s0 is offset, $t0 is memaddr or string (not starting w/length), $t1 is current digit, $s1 is the number itself, $t3 is strloc + offset		
		rem $t1, $s1, 10
		abs $t1, $t1
		add $t1, $t1, 48 #finds its ascii value
		add $t3, $t0, $s0
		sb $t1, ($t3) 
		div $s1, $s1, 10
		beq $s1, 0, end_while_string_make
		sub $s0, $s0, 1
		j while_string_make
	end_while_string_make:
	blt $s2, 0, neg_sub
	j end_neg_sub
	neg_sub:
		sub $t0, $t0, 1
	end_neg_sub:
	sub $t0, $t0, 4	
	move $v0, $t0
	lw $s2, 16($sp)
	lw $s1, 12($sp)
	lw $s0, 8($sp)
	lw $ra, 4($sp)
	add $sp, $sp, 16
	jr $ra

a_concat:
	sub $sp, $sp, 12
	sw $ra, 4($sp)
	sw $s0, 8($sp)
	sw $s1, 12($sp)
	move $s0, $a0 #s0 holds a pointer to s1
	move $s1, $a1 #s1 holds a pointer to s2
	lw $t0, ($a0)	
	lw $t1, ($a1)
	add $a0, $t0, $t1
	add $a0, $a0, 5
	sub $sp, $sp, 16
	jal malloc
	add $sp, $sp, 16
	lw $t0, ($s0)
	lw $t1, ($s1)
	add $t0, $t0, $t1 
	sw $t0, ($v0) # we store the length of the string in the newly created string
	move $t0, $v0 # $t0 now holds the address to the newly created string
	add $t0, $t0, 4 # $t0 now points to where we will put the characters
	add $s0, $s0, 4 # $s0 now points to the characters in s1
	add $s1, $s1, 4 # $s1 not points to the characters in s2
	while_s1:
		lbu $t1, ($s0)
		beq $t1, 0, end_while_s1
		lb $t1, ($s0)
		sb $t1, ($t0)
		add $s0, $s0, 1
		add $t0, $t0, 1
		j while_s1
	end_while_s1:
	while_s2:
		lbu $t1, ($s1)
		beq $t1, 0, end_while_s2
		lbu $t1, ($s1)
		sb $t1, ($t0)
		add $s1, $s1, 1
		add $t0, $t0, 1
		j while_s1
	end_while_s2:
	sb $0, ($t0)
	lw $s1, 12($sp)
	lw $s0, 8($sp)
	lw $ra, 4($sp)
	add $sp, $sp, 12
	jr $ra
	
a_substring:
	sub $sp, $sp, 16
	sw $ra, 4($sp)
	sw $s0, 8($sp)
	sw $s1, 12($sp)
	sw $s2, 16($sp)
	lw $s0, ($a0)
	blt $a1, 0, bad_args
	blt $a2, 0, bad_args
	bge $a1, $s0, bad_args #this and the previous line make sure the index is in bounds for the string
	add $t0, $a1, $a2
	bgt $t0, $s0, bad_args
	j end_bad_args
	bad_args:
		li $a0, 1
		jal a_error
	end_bad_args:
	move $t0, $a2
	move $s0, $a0
	move $s1, $a1
	move $s2, $a2
	add $a0, $t0, 5
	sub $sp, $sp, 16
	jal malloc
	add $sp, $sp, 16
	sw $s2, ($v0)
	move $t0, $v0
	add $t0, $t0, 4 #position in the new string to start adding characters
	add $s0, $s0, 4
	add $s0, $s0, $s1 #offset in the original str when we begin computing the substring
	while_nonempty_str:
		beq $s2, 0, end_while_nonempty_str
		lbu $t1, ($s0)
		sb $t1, ($t0)
		add $t0, $t0, 1
		add $s0, $s0, 1
		sub $s2, $s2, 1
		j while_nonempty_str
	end_while_nonempty_str:
	sb $0, ($t0)
	lw $s2, 16($sp)
	lw $s1, 12($sp)
	lw $s0, 8($sp)
	lw $ra, 4($sp)
	add $sp, $sp, 16
	jr $ra
	