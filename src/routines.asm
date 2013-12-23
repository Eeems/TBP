;Renders the text animation and text box to screen and handles text reading stuffs
;inputs: hl=text_pointer
Text_Output:
	call tobackbuff
___TORevert:
	push hl
		rectangle(0,43,95,63,0)
		box(0,43,95,63,1)
		ld a,44
		ld (penRow),a
		ld a,2
		ld (penCol),a
	pop hl
___TOloop:
	call IsKey
		call z, Slowdown
	ld a, (hl)
	inc hl
	cp '\n'
		jr z, ___TOWrapLine
	cp '\r'
		jr z,___TOPage
	or a
		jr z, __TOEnd
	b_call _VPutMap
	call IsKey
		call z,Fastcopys
	ld a,(penCol)
	cp 92
		jr nc,___TOWrapLine
	cp 86
		jr nc,+_
	jr ___TOloop
_	ld a,(penRow)
	push af
	cp 51
		jr nc,+_
	pop af
	jr ___TOloop
___TOWrapLine:
	push af
		ld a,2
		ld (penCol),a
		ld a,(penRow)
		add a, 6
		cp 57
			jr nc,_
		ld (penRow),a
	pop af
	jr ___TOloop
___TOPage:
	push af
_:
		push hl
			sprite(87,55,8,parse_pause_sprite)
			lcdupdate
			call _
			rectangle(0,43,95,63,0)
			box(0,43,95,63,1)
		pop hl
	pop af
	jp ___TORevert
__TOEnd:
	push hl
		sprite(87,55,8,parse_pause_sprite)
		lcdupdate
		call frombackbuff
		call _
		lcdupdate
	pop hl
	ret
_	;debug
	call FlushKey
	call Pause
	ret


;-----> DrawCustomRectangle
;input:	h=x1 l=y2 d=x2 e=y2 a=color
;output:none
DrawCustomRectangle:
api_draw_square:
  ld (api_line_color),a
api_draw_square_noset:
  ld a,h
  cp 96
  jr c,api_drawblock_skip1
  ld h,96
api_drawblock_skip1:
  ld a,d
  cp 96
  jr c,api_drawblock_skip2
  ld d,96
api_drawblock_skip2:
  ld a,l
  cp 64
  jr c,api_drawblock_skip3
  ld l,64
api_drawblock_skip3:
  ld a,e
  cp 64
  jr c,api_drawblock_loop
  ld e,64
api_drawblock_loop:
  ld a,l
  cp e
  ret nc
  push hl
  push de
  ld e,l
  call api_drawline_dontset
  pop de
  pop hl
  inc l
  ld a,(api_line_color)
  cp %10101010
  jr z,api_drawblock_dither
  cp %01010101
  jr nz,api_drawblock_loop
api_drawblock_dither:
  cpl
  ld (api_line_color),a
  jp api_drawblock_loop
api_drawline_set:
  ld (api_line_color),a
api_drawline_dontset:
  push hl
  ld a,h
  call api_drawline_getmask
  ld b,a
  ld a,d
  call api_drawline_getmask
  cpl
  ld c,a
  pop hl
  push bc
  call api_drawline_getbyte
  push hl
  ld h,d
  ld l,e
  call api_drawline_getbyte
  pop de
  push de
  push hl
  sbc hl,de
  jp z,api_drawline_onebyte
  ld a,l
  cp 1
  jp z,api_drawline_skip_inner
  ld b,l
  dec b
  ex de,hl
  inc hl
  ld a,(api_line_color)
api_drawline_loop_inner:
  ld (hl),a
  inc hl
  djnz api_drawline_loop_inner
api_drawline_skip_inner:
  pop de
  pop hl
  pop bc
  call api_drawline_modify
  ex de,hl
  ld b,c
  jp api_drawline_modify
api_drawline_onebyte:
  pop de
  pop hl
  pop bc
  ld a,b
  and c
  ld b,a
api_drawline_modify:
  push bc
  ld a,(api_line_color)
  push bc
  ld c,a
  ld a,b
  and c
  pop bc
  push af
  ld a,b
  cpl
  and (hl)
  ld b,a
  pop af
  or b
  ld (hl),a
  pop bc
  ret
api_drawline_getbyte:
  ld a,h
  ld h,0
  ld b,h
  ld c,l
  add hl,hl
  add hl,bc
  add hl,hl
  add hl,hl
  ld c,a
  srl c
  srl c
  srl c
  add hl,bc
  ld bc,gbuf
  add hl,bc
  ret
api_drawline_getmask:
  and 7
  ld hl,api_drawline_lut
  add a,l
  ld l,a
  ld a,(hl)
  ret
api_drawline_lut:
  .db %11111111
  .db %01111111
  .db %00111111
  .db %00011111
  .db %00001111
  .db %00000111
  .db %00000011
  .db %00000001
api_line_color:
	db 0
;--------------------------------
;Clip Big Sprite
;by James Montelongo
;MAX SIZE: 64x64
;ix - Sprite
;b  - height
;c  - width in bytes
;d  - x
;e  - y

;copies the gbuf to the backbuffer
tobackbuff:
	push hl
		push de
			push bc
				ld bc,767
				ld hl,AppBackupScreen
				ld d,h
				ld e,l
				ld hl,gbuf
				ldir
			pop bc
		pop de
	pop hl
	ret
;copies the backbuffer to gbuf
frombackbuff:
	push hl
		push de
			push bc
				ld bc,767
				ld hl,gbuf
				ld d,h
				ld e,l
				ld hl,AppBackupScreen
				ldir
			pop bc
		pop de
	pop hl
	ret

;slows down stuff
Slowdown:
	ld c,$0F
_		ld b,$FF
_			push bc
			pop bc
			djnz -_
		dec c
		ld a,c
		or a
			jr nz,--_
	ret