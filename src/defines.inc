;value replacement
gbuf			equ		PlotsScreen	;768  bytes
Text_buffer		equ		saferam2	;531  bytes
CurrentContrast equ		saferam3	;2    bytes
MAPUNCRNCH		equ		$8000		;1024 bytes
#DEFINE			dw		.dw			;
ProgStart		equ		$9D95		;
#DEFINE homeup push af \ xor a \ ld (penCol),a \ ld (penRow),a \ pop af