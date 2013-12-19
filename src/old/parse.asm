;Parser defines
#DEFINE Pend db 0	;end of the cutscene
#DEFINE Pclearscreen db 1	;clears the screen buffer
#DEFINE Pupdatescreen db 2	;updates the screen from the buffer
#DEFINE Ptext(text) db 3,text,0	;displays text on the screen [x,y,text,0]
#DEFINE Psprite(spritex,spritey,pointer,type)  db 4,spritex,spritey \ dw pointer \ db type	;displays a sprite on the screen of types 0=(8*8) 1=(8*16) 2=(8*16 masked) [x,y,pointer,type]
	;sprite types
P8x8 .equ 0
P8x16 .equ 1
P8x16m .equ 2
	;end sprite types
#DEFINE Ppause db 5	;waits for a key to be pressed and then depressed (updates screen)
#DEFINE Pwait(waittime) db 6,waittime	;waits n eighth seconds [n]
#DEFINE Prect(R1x,R1y,R2x,R2y,color) db 7,R2x,R2y,color,R1x,R1y	;draws a rectangle on the screen
#DEFINE Pnull db 8	;do nothing, waste a few cycles
#DEFINE Pbackground(pointer,spriteset) db 9 \ dw spriteset,pointer	;draws the fullscreen image [pointer]
#DEFINE Pstobuff db 10	;copies the buffer to the backbuffer
#DEFINE Prebuff db 11	;copies the backbuffer to the buffer

#DEFINE command(commandname,callname) cp commandname \ push af \ call z,callname \ pop af \ jr z,parse_cutscene_loop

;input: hl=cutscene
parse_cutscene:
	call ClearFull
parse_cutscene_loop:
	inc hl
	ld a,(hl)
	or a
		jr z,parse_end
	command(1,ClearBuff)	;Pclearscreen
	command(2,Fastcopys)	;Pupdatescreen
	command(3,parse_text)	;Ptext
	command(4,parse_sprite)	;Psprite
	command(5,parse_pause)	;Ppause
	command(6,parse_wait)	;Pwait
	command(7,parse_rect)	;Prect
	command(9,parse_background)	;Pbackground
	command(10,tobackbuff)	;Pstobuff
	command(11,frombackbuff)	;Prebuff
	cp 8	;Pnull
		jr z,parse_cutscene_loop
	;if not the correct token
	;tell the author
	;and disp token number found
	homeup
	ld b,(hl)
	ld hl,error_parse
	call DrawWrappedString
	call Newline
	ld a,b
	call VDispA
	lcdupdate
	call Pause
	jp parse_cutscene_loop
parse_end:
	call FadeWhite
	lcdclearall
	call resContrast
	ret
	
parse_background:
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
	pop ix	;pointer to sprite data
	inc hl
	ld e,(hl)
	inc hl 
	ld d,(hl) ;pointer to background data
	push hl
		ld h,d
		ld l,e
		ld de,MAPUNCRNCH
		call Uncrunch
		ld de,MAPUNCRNCH
	pop hl
	push hl
		ld a,0
		ld l,0
		call DrawTilemap
	pop hl
	ret
	
parse_rect:
	inc hl
	ld d,(hl)	;R2x
	inc hl
	ld e,(hl)	;R2y
	inc hl
	ld a,(hl)	;color
	inc hl
	ld a,(hl)	;get R1x
	inc hl
	push hl
		ld h,a	;store R1x
		ld l,(hl)	;R2y
		call DrawCustomRectangle
	pop hl
	ret
	
parse_pause:
	push hl
		sprite(87,56,8,parse_pause_sprite)
		lcdupdate
		call Pause
		sprite(87,56,8,parse_pause_sprite)
	pop hl
	ret
	
;waits for about 1/8 seconds
parse_wait:
	inc hl	;6
	ld c,(hl)	;7
_		ld a,$80	;7
_			ld b,$FF	;7
_				push hl	;11
				pop hl	;10
				djnz -_	;13/8
			dec a	;4
			or a	;4
				jr nz,--_	;12/7
		dec c	;4
		cp c	;4
			jr nz,---_	;12/7
	ret	;10
	
parse_sprite:
	inc hl
	ld d,(hl)	;x
	inc hl
	ld e,(hl)	;y
	inc hl
	push de
		ld e,(hl)
		inc hl
		ld d,(hl)
		push de
		pop ix
	pop de ;pointer
	inc hl
	ld a,(hl)	;type
	cp P8x16m
		jr z,+++_	;8*16 masked
	cp P8x16
		jr z,_	;8*16
	ld b,8	;8*8
	jr ++_
_	ld b,16
_	ld a,d
	push hl
		ld l,e
		push bc
			call IPutSprite
		pop bc
	pop hl
	ret
_	push hl
		push ix
		pop hl
		call putmaskedSprite
	pop hl
	ret
	
parse_text:
	inc hl
	call Text_Output
	dec hl
	ret