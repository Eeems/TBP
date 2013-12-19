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
;saves the savefile
file_save:
	call tobackbuff	;store the screen to the backbuffer
	homeup	;position penCol/Row at top left hand side of the screen
	ld hl,save_appvar_name	;load the appvar savename into hl
	rst 20h	;no idea
	b_call _chkfindsym	;check if it exists
		jr nc,++_	;if it exists jump to archive check
	ld hl,DefaultSettingsEnd-DefaultSettings	;load size into hl
	b_call _createappvar	;create the appvar
_	inc de	;increase past
	inc de	;the size bytes
	ld hl,SaveStart	;load the data location to save from
	ld bc,DefaultSettingsEnd-DefaultSettings	;and the size
	ldir	;copy data
	wrappedtext(saved_text)	;display save message
	lcdupdate	;update the screen
	call Pause	;pause till keypress
	call frombackbuff	;restore screen from backbuffer
	ret
_	xor a	;archive check set a to 0
	cp b	;if b is zero (unarchived)
		jr z,--_	;go to save
	ld hl,save_appvar_name	;otherwise store the name to hl
	rst 20h	;no clue why this is here
	b_call _Arc_Unarc	;unarchive the appvar
	jr --_	;go to save

;displays the name menu used to change the characters name
name_menu:
	call tobackbuff
	xor a	;set a 0
	ld (_cursor),a
	rectangle(5,4,75,61,0) \ rectangle(4,5,76,60,0)
	largeSprite(4,4,9,57,MenuImg)
	ld hl,(6*256)+6	;set x
	ld (penCol),hl	;and y loc
	wrappedtext(NameMenuTitle)
	ld a,6
	ld (penCol),a
	ld a,28
	ld (penRow),a
	wrappedtext(NameMenuItems)
name_menu_loop:
		call name_menu_disp
		b_call _getCSC
		cp skRight
			jr z,name_menu_cr
		cp skLeft
			jr z,name_menu_cl
		cp skUp
			jr z,name_menu_cu
		cp skDown
			jr z,name_menu_cd
		cp skMode
			jr z,_
		cp skClear
			jr z,_
		cp skGraph
			jr nz,name_menu_loop
_	xor a	;set a 0
	ld (_cursor),a
	call frombackbuff
	ret
name_menu_disp:
	rectangle(6,16,74,25,0)	;display name string and cursor
	ld a,19
	ld (penRow),a
	ld a,(_cursor)	;get cursor position
	ld c,a
	add a,a
	add a,c
	add a,a	;mult by 6
_	add a,7	;add standard 7
	ld (penCol),a	;store position
	ld a,'_'
	b_call _VPutMap
	ld a,16
	ld (penRow),a
	ld a,7
	ld (penCol),a
	monospacetext(Name)
	lcdupdate
	ret
name_menu_cr:
	ld a,(_cursor)
	cp 10
		jr z,name_menu_loop
	inc a
	ld (_cursor),a
	jr name_menu_loop
name_menu_cl:
	ld a,(_cursor)
	or a
		jr z,name_menu_loop
	dec a
	ld (_cursor),a
	jr name_menu_loop
name_menu_cu:
	ld hl,Name
	ld a,(_cursor)
	ld e,a
	ld d,0
	add hl,de
	ld a,(hl)
	cp 122
		jp z,name_menu_loop
	inc a
	ld (hl),a
	jp name_menu_loop
name_menu_cd:
	ld hl,Name
	ld a,(_cursor)
	ld e,a
	ld d,0
	add hl,de
	ld a,(hl)
	cp 32
		jp z,name_menu_loop
	dec a
	ld (hl),a
	jp name_menu_loop
;32-122 is (space)-z

;displays the contrast menu used to change the contrast
contrast_menu:
	call tobackbuff
	rectangle(5,4,75,61,0) \ rectangle(4,5,76,60,0)
	largeSprite(4,4,9,57,MenuImg)
	ld a,6					;set
	ld (penCol),a			;x and
	ld (penRow),a			;y loc
	wrappedtext(ContrastMenuTitle)
	call ++_
	ld a,16
	ld (penRow),a
	wrappedtext(ContrastMenuItem0)
	ld a,(currentContrast)
	call VDispA
	call ++_
	ld a,22
	ld (penRow),a
	wrappedtext(ContrastMenuItem1)
	lcdupdate
_		getkey(dkAdd)
			jr nz,ContrastInc
		getkey(dkUp)
			jr nz,ContrastInc
		getkey(dkRight)
			jr nz,ContrastInc
		getkey(dkSub)
			jr nz,ContrastDec
		getkey(dkDown)
			jr nz,ContrastDec
		getkey(dkLeft)
			jr nz,ContrastDec
		getkey(dkClear)
			jr z,-_
	call frombackbuff
	ret
_	ld a,6
	ld (penCol),a
	ret
ContrastInc:
	ld a,(currentContrast)
	inc a
	cp 40
		jp z,contrast_menu
	ld (currentContrast),a
	call setContrast
	jp contrast_menu
ContrastDec:
	ld a,(currentContrast)
	dec a
	or a
		jp z,contrast_menu
	ld (currentContrast),a
	call setContrast
	jp contrast_menu
;sets the contrast to the value in a
setContrast:
	push af
		ld (contrast),a
		add a,$D8
		or %11000000
		out ($10),a
	pop af
	ret
;resets contrast back to original setting
resContrast:
	ld a,(CurrentContrast)
	call setContrast
	ret
;fades contrast till white
FadeWhite:
	push af
		ld a,1
		ld (CurrentContrast+1),a
_			ld hl,CurrentContrast
			call parse_wait
			ld a,(contrast)
			dec a
			call setContrast
			cp 5
				jr nz,-_
	pop af
	ret
;draws a menu based on a table.
MenuRoutine:
	call Flushkey
	call tobackbuff	;store to backbuffer to save screen
	push hl	;store input
		rectangle(5,4,75,61,0) \ rectangle(4,5,76,60,0)			;clear the box beneath the menu
		largeSprite(4,4,9,57,MenuImg)	;draw the menu item
	pop hl	;recall the input
	ld a,6					;set
	ld (penCol),a			;x and
	ld (penRow),a			;y loc
	call DrawWrappedString	;draw it
	ld b,(hl)				;store value to b for loop
	ld c,b					;current selected position
	inc hl					;increase to the string
	ld a,11					;store the correct location
	ld (penRow),a			;to y
menudisploop:
		ld a,16					;set
		ld (penCol),a			;x
		ld a,(penRow)			;get y
		add a,6					;go to newline
		ld (penRow),a			;store y
		call DrawWrappedString	;draw it
		djnz menudisploop		;loop until b is 0
	ld b,c		;store amount of items back to b
	ld c,0		;set c to first item
menukeyloop:
	call menupickdraw	;draw the menu selector
	lcdupdate			;update the screen
	call Pause		;wait until key is pressed
	call menupickdraw	;erase the menu selector
	cp skUp
		jr z,menuup
	cp skDown
		jr z,menudown
	cp skClear
		jr z,menucancel
	cp sk2nd
		jr z,_
	cp skEnter
		jr nz,menukeyloop
_	call frombackbuff	;restore original screen
	ld a,c				;check to see if c is the first item
	or a				;skip next loop if so
		jr z,menujump
	ld b,c				;b is the selected item
menucancel:
		inc hl			;increase to
		inc hl			;the next item in the jump list
		djnz menucancel	;repeat until at the right item
menujump:
	call Flushkey
	ld a,(hl)	;store the value of the first part into a
	inc hl		;increate to the second half of the address
	ld h,(hl)	;store the second part to h
	ld l,a		;and the first to l
	jp (hl)		;jump now to the address in hl
menudown:
	ld a,c					;get the current item
	inc a					;increase it
	cp b					;if it is the max
		jr z,menukeyloop	;skip next step
	inc c					;otherwise actually increase it
	jr menukeyloop			;then jump back
menuup:
	ld a,c					;get current item
	cp 0					;if it is zero
		jr z,menukeyloop	;do nothing to it
	dec c					;otherwise decrease it
	jr menukeyloop			;then jump back
menupickdraw:
	push bc							;don't want to effect
		push af						;all the registers
			push hl					;so lets push them onto the stack
				ld a,6				;lets now set the correct x location
				call menusetl		;and y
				ld b,8				;and height
				ld ix,menupicker	;and give it the right sprite
				call IPutSprite		;draw it now!
			pop hl					;and now we restore
		pop af						;all the registers
	pop bc							;back to normal that we need
	ret								;routine done!
menusetl:
	push af				;protect af
		ld a,c			;because we are puttin the current item into it
		or a			;checking if it is zero
			jr z,++_	;if so jump forward two
		xor a			;otherwise set a to zero for some math
		push bc			;protect current bc
			ld b,c		;load c into b
_:
				add a,6	;add 6 to a until done with b
				djnz -_	;loop de loop
		pop bc			;lets get the original b back
_:
		add a,16		;add the standard 16 offset
		ld l,a			;store it all to e
	pop af				;reset a
	ret					;done setting e
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
;outputs the string in HL at penCol and penRow and wraps it on the screen. It also allows for manual newlines with \n
DrawWrappedString:
	ld a, (hl)
	inc hl
	cp '\n'
		jr z, WrapLine
	cp '\r'
		jr z, WrapPage
	cp '\t'
		jr z,Tab
	or a
		ret z
	b_call _VPutMap
	ld a,(penCol)
	cp 92
		jr nc,WrapLine
	jr DrawWrappedString
Tab:
	ld a,(penCol)
	add a,6
	ld (penCol),a
	cp 55
		call nc,Newline
	jr DrawWrappedString
WrapLine:
	call Newline
	jr DrawWrappedString
WrapPage:
	lcdupdate
	call pause
	clearlcd
	homeup
	jr DrawWrappedString
;monospace text
DrawMonospaceString:
	ld a, (hl)
	inc hl
	cp '\n'
		jr z, monoWrapLine
	cp '\r'
		jr z, monoWrapPage
	cp '\t'
		jr z,monoTab
	or a
		ret z
	push af
		ld a,(penCol)
		ld b,a
	pop af
	push bc
		b_call _VPutMap
	pop bc
	ld a,b
	add a,6
	ld (penCol),a
	cp 92
		jr nc,monoWrapLine
	jr DrawMonospaceString
monoTab:
	ld a,(penCol)
	add a,12
	ld (penCol),a
	cp 55
		call nc,Newline
	jr DrawMonospaceString
monoWrapLine:
	call Newline
	jr DrawMonospaceString
monoWrapPage:
	lcdupdate
	call pause
	clearlcd
	homeup
	jr DrawMonospaceString
;newline, wrapps screen if necessary
;preserves af
Newline:
	push af
		xor a
		ld (penCol),a
		ld a,(penRow)
		add a, 6
		cp 55
			jr nc,NewLine_top
		ld (penRow),a
	pop af
	ret
NewLine_top:
		xor a
		ld (penRow),a
	pop af
	ret
;clears the buffer and updates
ClearFull:
	clearlcd
	lcdupdate
	ret
;clears the graphbuffer
ClearBuff:
	push hl
		push de
			push bc
				ld hl,PlotSScreen
				ld (hl),0
				ld d,h
				ld e,l
				inc de
				ld bc,767
				ldir
			pop bc
		pop de
	pop hl
	ret
;########################################
;8*16 masked sprites
;input: de = xy
;	hl = pic start
putmaskedSprite:
	push	hl
	ld	a, d
	ld	d, 0
	ld	h, 0
	ld	l, e
	add	hl, hl
	add	hl, de
	add	hl, hl
	add	hl, hl
 
	ld	e, a
	srl	e
	srl	e
	srl	e
	add	hl, de
	ld	de, plotSScreen
	add	hl, de
 
	pop	ix
	and	7
	ld	c, a
	ld	b, 16
maskedLp:
	push	bc
	ld	de, 0
	ld	a, c
	or	a
	ld	b, c
	ld	a, (ix + 16)
	ld	c, (ix)
	jr	z, aligned
alignLp:
	rra		;neither side can bring a carry
	rr	d
	srl	c
	rr	e
	djnz	alignLp
aligned:
 
	cpl
	and	(hl)
	or	c
	ld	(hl), a
	inc	hl
	ld	a, d
	cpl
	and	(hl)
	or	e
	ld	(hl), a
 
	ld	de, 11
	add	hl, de
	inc	ix
	pop	bc
	djnz	maskedLp
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

ClipBigSprite:
; Early out, Check if its even remotely on screen
	ld a,e
	cp 64
	ret p
	add a,b
	ret m
	ret z
	ld a,d
	cp 96
	ret p
	ld a,c
	add a,a
	add a,a
	add a,a
	add a,d
	ret m
	ret z

	ld a,e
	or a
	jp p,Check_clip_bottom
	neg
	push de
	ld hl,0
	ld d,l
	ld e,a
	bit 2,c
	jr z,$+2+1
	add hl,de
	add hl,hl	
	bit 1,c
	jr z,$+2+1
	add hl,de
	add hl,hl	
	bit 0,c
	jr z,$+2+1
	add hl,de
	pop de
	ex de,hl
	add ix,de		;Here you can save the top offset
	ex de,hl
	ld e,0
	neg
	add a,b
	ld b,a
Check_clip_bottom:

	ld a,e
	add a,b
	sub 64
	jp m,Check_clip_Left
	neg
	add a,b
	ld b,a
Check_clip_Left:
				; at this point you may want to save b
	xor a
	ld (bigskip),a
	ld a,Clipleftsize 
	ld (Do_Clipleft),a
	ld a,d
	or a
	jp p,Check_clip_right
	cpl
	and $F8
	rra
	rra
	rra
	ex de,hl		;save the clipped left offset
	ld e,a
	ld d,0
	add ix,de
	ld (bigskip),a
	ex de,hl
	inc a
	neg
	add a,c
	ld c,a
	xor a
	ld (Do_Clipleft),a
	ld a,d
	and $07
	ld d,a
Check_clip_right:

	ld a,Cliprightsize 
	ld (Do_Clipright),a
	ld a,c
	add a,a
	add a,a
	add a,a
	add a,d
	sub 96
	jp m,Check_clip_middle
	and $F8
	rra
	rra
	rra
	ld l,a
	ld a,(bigskip)
	add a,l
	inc a
	ld (bigskip),a
	neg
	add a,c
	ld c,a
	xor a
	ld (Do_Clipright),a
Check_clip_middle:
				; This is where C should be saved.
	xor a
	ld (Do_ClipMiddle),a
	ld a,c
	or a
	jp nz,dontskipmiddle
	ld a,ClipMiddlesize 
	ld (Do_ClipMiddle),a
dontskipmiddle:
	ld l,e
	ld a,d	
	ld h,0
	ld d,h
	add hl,hl
	add hl,de
	add hl,hl
	add hl,hl
	ld e,a
	and $07
	xor 7
	ld (BigRot1),a
	ld (BigRot2),a
	ld (BigRot3),a
	add a,a
	ld (clipbigrot1),a
	ld a,$ff
clipbigrot1 = $+1
	jr $
	srl a
	srl a
	srl a
	srl a
	srl a
	srl a
	srl a
	srl e
	srl e
	srl e
	add hl,de
	ld de,gbuf
	add hl,de
				; This is where gbuf offset should be saved.
	ld d,a
	cpl
	ld e,a
				;masks should be saved to
BigSpriteRow:
	push bc
	push hl
	ld b,c
Do_Clipleft = $+1
	jr Clipleft
	ld a,(ix)
	inc ix
BigRot1 = $+1
	jr $
	rrca
	rrca
	rrca
	rrca
	rrca
	rrca
	rrca
BigMask0:
	and e
	or (hl)
	ld (hl),a
Clipleft:
Clipleftsize = Clipleft-(Do_Clipleft+1)

Do_ClipMiddle = $+1
	jr $+2
BigSpriteloop:
	ld a,(ix)
	inc ix
BigRot2 = $+1
	jr $
	rrca
	rrca
	rrca
	rrca
	rrca
	rrca
	rrca
	ld c,a
BigMask1:
	and d
	or (hl)
	ld (hl),a
	inc hl
	ld a,c
BigMask2:
	and e
	or (hl)
	ld (hl),a
	djnz BigSpriteloop
ClipMiddle:
ClipMiddlesize = ClipMiddle-(Do_ClipMiddle+1)

Do_ClipRight = $+1
	jr ClipRight
	ld a,(ix)
BigRot3 = $+1
	jr $
	rrca
	rrca
	rrca
	rrca
	rrca
	rrca
	rrca
BigMask3:
	and d
	or (hl)
	ld (hl),a
ClipRight:
Cliprightsize = ClipRight-(Do_ClipRight+1)
	pop hl

	ld bc,12			;width of the screen
	add hl,bc

bigskip = $+1
	ld bc,0
	add ix,bc
	pop bc
	djnz BigSpriteRow
	ret
clearTextBuff:	;clears the textbuffer completely for things that don't know if they are really going to write to it.
	push bc
		push de	;poop
			push hl
				ld hl,text_buffer
				ld de,text_buffer+1
				ld bc,531-1
				ld (hl),0
				ldir
			pop hl
		pop de
	pop bc
	ret
