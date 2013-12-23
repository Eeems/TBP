#define	dkDown		$FE01
#define	dkLeft		$FE02
#define	dkRight		$FE04
#define	dkUp		$FE08
#define	dkEnter		$FD01
#define	dkAdd		$FD02
#define	dkSub		$FD04
#define	dkMul		$FD08
#define	dkDiv		$FD10
#define	dkPower		$FD20
#define	dkClear		$FD40
#define	dkChs		$FB01
#define	dk3			$FB02
#define	dk6			$FB04
#define	dk9			$FB08
#define	dkRParen	$FB10
#define	dkTan		$FB20
#define	dkVars		$FB40
#define	dkDecPnt	$F701
#define	dk2			$F702
#define	dk5			$F704
#define	dk8			$F708
#define	dkLParen	$F710
#define	dkCos		$F720
#define	dkPrgm		$F740
#define	dkStat		$F780
#define	dk0			$EF01
#define	dk1			$EF02
#define	dk4			$EF04
#define	dk7			$EF08
#define	dkComma		$EF10
#define	dkSin		$EF20
#define	dkMatrix	$EF40
#define	dkApps		$EF40
#define	dkGraphvar	$EF80
#define	dkStore		$DF02
#define	dkLn		$DF04
#define	dkLog		$DF08
#define	dkSquare	$DF10
#define	dkRecip		$DF20
#define	dkMath		$DF40
#define	dkAlpha		$DF80
#define	dkGraph		$BF01
#define	dkTrace		$BF02
#define	dkZoom		$BF04
#define	dkWindow	$BF08
#define	dkYEqu		$BF10
#define	dk2nd		$BF20
#define	dkMode		$BF40
#define	dkDel		$BF80

;-----> Direct key input
;Input: bc=key
;Output: a,hl,z=key state
dKey:
	ld	a,b
	out	($01),a
	xor	a
	ld	h,a
	ld	l,a
	ld	a,(hl)
	in	a,($01)
	xor	c
	and	c
	ret	z
	inc	l
	ld	a,l
	ret
;waits until all keys have been released
Flushkey:
		ld a,$FF \ out ($01),a
		xor a
		out ($01),a
		push af
		pop af	;delay
		in a,($01)
		inc a
			jr nz,$-3
	ret
;returns nz if a key is pressed
IsKey:
		ld a,$FF \ out ($01),a
		xor a
		out ($01),a
		push af
		pop af	;delay
		in a,($01)
		inc a
	ret