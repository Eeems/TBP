;The following routine subtracts b from hl and places the result in hl
BsL:
	push af
		push bc
			ld b,l
		pop af
		ld l,a
	pop af
_		dec l
		djnz -_
	ret
;The following routine multiplies h by e and places the result in hl
HmE:
	ld	l, 0
	ld	d, l
	sla	h		; optimised 1st iteration
	jr	nc, $+3
	ld	l, e
	ld b, 7
_:
		add	hl, hl          
		jr	nc, $+3
		add	hl, de
		djnz	-_
   ret
;The following routine divides hl by c and places the quotient in hl and the remainder in a 
HLdC:
	xor	a
	cp c
		ret z	;if c is 0 quit now
	ld	b, 16
_:
		add	hl, hl
		rla
		cp	c
		jr	c, $+4
		sub	c
		inc	l
		djnz -_
	ret