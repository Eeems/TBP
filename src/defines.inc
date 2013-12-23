;value replacement
gbuf			equ		PlotsScreen	;768  bytes
Text_buffer		equ		saferam2	;531  bytes
CurrentContrast equ		saferam3	;2    bytes
MAPUNCRNCH		equ		$8000		;1024 bytes
#DEFINE			dw		.dw			;
ProgStart		equ		$9D95		;
#DEFINE homeup push af \ xor a \ ld (penCol),a \ ld (penRow),a \ pop af
#DEFINE dialog(text_pointer) ld hl,text_pointer \ call Text_Output
#DEFINE dispPause call Fastcopys \ call Pause
#DEFINE rectangle(R1x,R1y,R2x,R2y,color) ld h,R1x \ ld l,R1y \ ld d,R2x \ ld e,R2y \ ld a,color \ call DrawCustomRectangle
#DEFINE box(R1x,R1y,R2x,R2y,color) ld h,R1x \ ld l,R1y \ ld d,R2x \ ld e,R2y \ ld a,color \ call Fastrectangle
#DEFINE sprite(Spritex,Spritey,Spriteh,pointer) ld a,Spritex \ ld l,Spritey \ ld b,Spriteh \ ld ix,pointer \ call IPutSprite
#DEFINE lcdupdate call Fastcopys