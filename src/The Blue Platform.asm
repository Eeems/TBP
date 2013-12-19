.NOLIST
#include "ti83plus.inc"
#include "dcs7.inc"
#include "defines.inc"
.LIST
.org ProgStart-2
.db $BB,$6D
	xor d
	ret
	jr Init
	.dw Description
	.db $07,$00
	.dw Icon
	.dw $0000
Description:
	db "Platformer RPG",0
Icon:
#include "icon.bmp"
Init:
	set TextWrite,(iy+SGrFlags)
	res DonePrgm,(iy+DoneFlags)
	bcall _runIndicOff		;turn off the run indicator, if I can read correctly :P
	xor a
	out ($20),a				;set to 6MHz mode
	homeup					;set penCol/Row to 0
	ld a,(contrast)
	ld (CurrentContrast),a	;get and store the contrast for later
Start:
	
	ret