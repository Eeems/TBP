Game_engine:
	push af
		ld de,MAPUNCRNCH
		ld hl,(mapD)
		call Uncrunch
		xor a
		ld (gameMode),a
		xor a
		ld (charJ),a
		ld (charJ+1),a
	pop af
	ld c,4
_:
		ld a,(BlackMageStats+health)
		or a
			ret z	;quit if no health
		call game_check_keys
		dec c
			jr nz,-_
_:
		ld c,4
		ld a,(mapy)
		ld l,a
		ld a,(mapx)
		ld ix,(mapD+2)	;get the current map tileset
		ld de,MAPUNCRNCH	;and the current map tilemap
		call DrawTilemap
		push bc
			ld a,(charS+2) \ ld hl,(charS+5) \ call game_get_sprite	;get the hl (sprite pointer)
			ld a,(charx) \ ld d,a \ ld a,(chary) \ ld e,a \ call putmaskedSprite	;draw it
			call char_dmg_check	;check for damage and put damagae alert
			ld a,(charS) \ dec a \ ld (charS),a	;decrease the timer by one
			or a	;if 0
				call z,game_switch_sprite	;switch sprite to use and reset timer
			dispA(87,0,BlackMageStats+health)
			dispA(87,6,BlackMageStats+mana)
;-------------------------------Debug Stuff-----------------------------
#IFDEF DEBUG_ENABLED
			homeup
			ld a,(charT) \ call VDispA \ ld a,(charT+1) \ call VDispA	;tile x,y
			call Newline
			getX \ AtoHL \ call VDispHL \ getY \ AtoHL \ call VDispHL	;pixel x,y
			call Newline
			ld a,(charJ) \ AtoHL \ call VDispHL \ ld a,(charJ+1) \ AtoHL \ call VDispHL	;plateu time/jumping bool, jumping time
#ENDIF
;-----------------------------------------------------------------------
			lcdupdate
		pop bc
		jp --_
	ret
VDispA:
	b_call _setxxop1
	ld a,8
	b_call _dispop1a
	ret

game_switch_sprite:
	ld a,3
	ld (charS),a
	ld a,(charS+1)
	xor 1	;swap sprites
	ld (charS+1),a
	cp 0
		jr nz,_	;if second sprite jump to loading
	ld a,(charS+3)	;load first in animation
	jr ++_	;skip second sprite loading
_	ld a,(charS+4)	;load second in animation
_	ld (charS+2),a	;store to current sprite
	ret
game_get_sprite:
;inputs:
;	a=sprite id
;	hl=base location
	push bc
		push de
			or a
				jr z,++_
			ld b,a
			ld de,32
_				add hl,de
				djnz -_
_		pop de
	pop bc
	ret
game_check_keys:
	push bc
	xor a \ ld (charS+3),a \ ld (charS+4),a	;reset animation
	call char_position_update
	getkey(dk2nd)
		call nz,char_jump
	call char_ladder
	call char_physics	;apply physics to character
	call char_position_update
	getkey(dkLeft)
		call nz,char_left
	call char_position_update
	getkey(dkRight)
		call nz,char_right
	getkey(dkClear)
		jr nz,engine_menu
	getkey(dkMode)
		jr nz,engine_menu
	getkey(dkGraph)
		jr nz,engine_menu
engine_return:
	pop bc
	ret
engine_name:
	call name_menu
engine_menu:	;clear
	menu(GameMenu)
engine_settings:
	menu(SettingsMenu)
engine_contrast:
	call contrast_menu
	jr engine_settings
engine_save:
	call file_save
	jr engine_return	;return to the game engine
engine_quit:
	ld sp,(initstack)
	jp Start
	
char_ladder:

	ld a,(charJ)
	or a
		ret nz	;quit if jumping
	
	ld a,(charT)	;get x
	ld d,a
	ld a,(charT+1)	;get y
	ld e,a
	
	call Tile_value	;check (x,y)
	cp 10
		jr z,++_
	
	inc e	;go to y+1
	
	call Tile_value
	cp 10
		jr z,++_	;check (x,y+1)
	
	getoffsetD
	ld d,a	;store it back to d
	getX	;get the characters edge
	cp d	;compare
		jr z,_	;if it doesn't equal the other, then it is partway between the two
				;so another check is required
	ld a,(charT)	;get x
	inc a	;get for x+1
	ld d,a
	
	call Tile_value	;check (x+1,y+1)
	cp 10
		jr z,++_
	
	dec e	;go to Y
	
	call Tile_value	;check (x+1,y)
	cp 10
		jr z,++_
		
_	xor a
	ld (charT+2),a
	ret	;quit if no ladders found
	
_	ld a,1
	ld (charT+2),a
	getkey(dkUp)
		jr z,_
	call char_up
	loadanim(charS,7,8)
	ret
_	loadanim(charS,7,7)
	getkey(dkDown)
		ret z
	call char_down
	loadanim(charS,7,8)
	ret
	
char_dmg_check:
	
	ld a,(charT)	;get x
	ld d,a
	ld a,(charT+1)	;get y
	ld e,a
	
	call Tile_value	;check (x,y)
	cp 6
		jr z,+_
	cp 23
		jr z,++_
	cp 22
		jp z,++++_
	cp 24
		jr z,+++_
	
	inc e	;go to y+1
	
	call Tile_value
	cp 6
		jr z,+_	;check (x,y+1)
	cp 23
		jr z,++_
	cp 22
		jp z,++++_
	cp 24
		jr z,+++_
	
	getoffsetD
	ld d,a	;store it back to d
	getX	;get the characters edge
	cp d	;compare
		ret z	;if it doesn't equal the other, then it is partway between the two
				;so another check is required
	ld a,(charT)	;get x
	inc a	;get for x+1
	ld d,a
	
	call Tile_value	;check (x+1,y+1)
	cp 6
		jr z,_
	cp 23
		jr z,++_
	cp 22
		jr z,++++_
	cp 24
		jr z,+++_
	
	dec e	;go to Y
	
	call Tile_value	;check (x+1,y)
	cp 6
		jr z,_
	cp 23
		jr z,++_
	cp 22
		jr z,++++_
	cp 24
		jr z,+++_
	ret
	
;apply damage
_	ld a,(BlackMageStats+health)
	dec a
	ld (BlackMageStats+health),a
	ld a,(charS+2) \ ld hl,damage_alert	;get the hl (sprite pointer)
	ld a,(charx) \ ld d,a \ ld a,(chary) \ ld e,a \ call putmaskedSprite	;draw it
	ret

;apply healing
_	ld a,(BlackMageStats+mhealth)
	ld b,a
	ld a,(BlackMageStats+health)
	cp	b
		ret z
	inc a
	ld (BlackMageStats+health),a
	ret
	
;apply mana loss
_	ld a,(BlackMageStats+mana)
	or a
		jr z,$+4
	dec a
	ld (BlackMageStats+mana),a
	ld a,(charS+2) \ ld hl,damage_alert	;get the hl (sprite pointer)
	ld a,(charx) \ ld d,a \ ld a,(chary) \ ld e,a \ call putmaskedSprite	;draw it
	ret

;apply mana regen
_	ld a,(BlackMageStats+mmana)
	ld b,a
	ld a,(BlackMageStats+mana)
	cp	b
		ret z
	inc a
	ld (BlackMageStats+mana),a
	ret

char_jump:
	
	ld a,(charJ)	;get the switch for if char is jumping
	or a	;check if it is
		ret nz	;if it is return
	
	ld a,(charT+1)	;get y
	inc a	;get y+1
	inc a	;get y+2
	ld e,a
	
	ld a,(charT)	;get x
	ld d,a
	
	call Tile_value	;get tile properties of (x,y+2)
	call TileType
		jr z,_	;if solid test position
	cp 10		;if ladder
		jr z,_	;test position
		
	inc d	;check x+1
	
	call Tile_value	;get tile properties of (x+1,y-1)
	call TileType
		jr z,++_	;if solid test x position
	cp 10		;if ladder
		jr z,_	;test x position
		
	
	ret	;otherwise do normal stuff
	
_	;test position
	dec e 	;decrease y back to characters tile
	getoffsetE	;get tiles edge
	ld e,a	;store it back to d
	getY	;get the characters edge
	add a,16
	cp e	;compare
		jr nc,++_	;if it is more then one past then it is right up against the edge, or it is in the tile

_	;test x position
	getoffsetD	;get tiles edge
	ld d,a	;store it back to d
	getX	;get the characters edge
	add a,8
	cp d	;compare
		ret z	;if it is more then one past then it is right up against the edge, or it is in the tile

_	ld a,(BlackMageStats+hover)	;get jump pleteu value
	ld (charJ),a	;store it
	ld a,(BlackMageStats+jump)	;store jump stats to a
	ld (charJ+1),a	;store that value to the jump counter
	ret

char_physics:
	ld a,(BlackMageStats+hover)	;get plateu time
	ld b,a	;store it to b
	ld a,(charJ)	;get the switch for if char is jumping
	cp b	;if it is not the plateu time
		jr nz,++_	;decrease
	or a	;check if it is 0
		jr z, +++_	;if so, apply gravity and don't return to this check
_	call char_up	;otherwise jump and return
		jr nz,+++_	;if it fails reset the counter
	ld a,(charJ+1)	;get the counter
	dec a	;decrease it
	ld (charJ+1),a	;store it back
	or a	;check if it's zero
		ret nz	;if lower value
	ld a,(charJ)

_	or a	;check if 0
		jr z,_	;if so, go forward to ladder check
	dec a	;otherwise decrease a
	ld (charJ),a	;and store it back to the plateu counter
	ret	;return
	
_	ld a,(charT+2)	;check if on a ladder
	or a	;if not
		jp z,char_down	;fall
	ret
	
_	ld a,1	;set a to 0
	ld (charJ+1),a	;turn off jump counter
	ld a,(charJ)	;get 
	jr --_

;stores the current character locations to the appropriate ram locations
;inputs: none
;outputs: none
;destroys: af
char_position_update:
	gettileX	;get the tile x locaton (0-32)
	ld (charT),a	;store it to RAM location for easy recall
	gettileY	;get the tile  location (0-32)
	ld (charT+1),a	;store it to the appropriate RAM location
	ret
	
char_up:	;up
	getY \ dec a \ srl a \ srl a \ srl a
	;dec a	;get y-1
	ld e,a
	
	ld a,(charT)	;get x
	ld d,a
	
	call Tile_value	;get tile properties of (x,y-1)
	call TileType
		jr z,_	;if solid test position
	
	inc d	;check x+1
	
	call Tile_value	;get tile properties of (x+1,y-1)
	call TileType
		jr z,++_	;if solid test x position
	
	jr +++_	;otherwise do normal stuff
	
_	;test position
	inc e	;decrease y back to characters tile
	getoffsetE	;get tiles edge
	ld e,a	;store it back to d
	getY	;get the characters edge
	cp e	;compare
		jp c,char_up_false	;if it is more then one past then it is right up against the edge, or it is in the tile

_	;test x position
	getoffsetD	;get tiles edge
	ld d,a	;store it back to d
	getX	;get the characters edge
	add a,8
	cp d	;compare
		jp nz,char_up_false	;if it is more then one past then it is right up against the edge, or it is in the tile
		
_	ld a,(mapy)
	or a	;if not at edge of screen
		jr z,++_
	cp 192
		jr z,+++_
	ld a,(chary)
	cp 24	;if centered
		jr nz,++_
	;move screen
_	ld a,(mapy)
	or a
		jp z,char_up_false
	dec a
	ld (mapy),a
	jp char_up_true
	;move char
_	ld a,(chary)
	or a
		jr z,char_up_false
	dec a
	ld (chary),a
	jr char_up_true
	;check to see if return handle to screen
_	ld a,(chary)
	cp 23
		jr nc,---_
	jr --_
char_up_false:
	ld a,1	;set a to 1
	or a	;set flags to nz
	ret
char_up_true:
	xor a	;set a to 0
	or a	;set flags to z
	ret

char_down:	;down
	ld a,(charT+1)	;get y
	inc a	;get y+1
	inc a	;get y+2
	ld e,a
	
	ld a,(charT)	;get x
	ld d,a
	
	call Tile_value	;get tile properties of (x,y+2)
	call TileType
		jr z,_	;if solid test position
		
	inc d	;check x+1
	
	call Tile_value	;get tile properties of (x+1,y-1)
	call TileType
		jr z,++_	;if solid test x position
	
	jr +++_	;otherwise do normal stuff
	
_	;test position
	dec e 	;decrease y back to characters tile
	getoffsetE	;get tiles edge
	ld e,a	;store it back to d
	getY	;get the characters edge
	add a,16
	cp e	;compare
		ret nc	;if it is more then one past then it is right up against the edge, or it is in the tile

_	;test x position
	getoffsetD	;get tiles edge
	ld d,a	;store it back to d
	getX	;get the characters edge
	add a,8
	cp d	;compare
		ret nz	;if it is more then one past then it is right up against the edge, or it is in the tile
	
_	ld a,(mapy)
	or a	;if not at edge of screen
		jr z,+++_
	cp 192
		jr z,++_
	ld a,(chary)
	cp 24	;if centered
		jr nz,++_
	;move screen
_	ld a,(mapy)
	cp 192
		ret z
	inc a
	ld (mapy),a
	ret
	;move char
_	ld a,(chary)
	cp 48
		ret z
	inc a
	ld (chary),a
	ret
	;check to see if return handle to screen
_	ld a,(chary)
	cp 23
		jr nc,---_
	jr --_
char_right:	;right
	ld a,(charT)	;get x
	inc a	;get x+1
	ld d,a
	
	ld a,(charT+1)	;get y
	ld e,a
	
	call Tile_value	;get tile properties of (x+1,y)
	call TileType
		jr z,_	;if solid test position
	
	inc e	;get y+1
	
	call Tile_value	;get tile properties
	call TileType
		jr z,_	;if solid test position
		
	dec e	;get back to y
	ld a,e \ add a,a \ add a,a \ add a,a	;get the offset
	ld e,a	;put it in e
	getY	;get the Y offset of the char
	cp e	;compare with e
		jr z,++_	;if it is equal
	
	ld a,(charT)	;load the x tile loc
	inc a	;get to x+1
	ld d,a	;store to d
	ld a,(charT+1)	;get the y tile loc
	inc a \ inc a	;get to y+2
	ld e,a	;store to e
	call Tile_value	;find out what tile it is
	call TileType	;get the properties
		ret z	;return if solid
	
	jr ++_	;otherwise do normal stuff
	
_	;test position
	dec d	;decrease x back to characters tile
	getoffsetD	;get tiles edge
	ld d,a	;store it back to d
	getX	;get the characters edge
	cp d	;compare
		ret nc	;if it is more then one past then it is right up against the edge, or it is in the tile
		
_	loadanim(charS,5,6)
	ld a,(mapx)
	or a	;if not at edge of screen
		jr z,+++_
	cp 168
		jr z,++_
	ld a,(charx)
	cp 38	;if centered
		jr nz,++_
	;move screen
_	ld a,(mapx)
	cp 168
		ret z
	inc a
	ld (mapx),a
	ret
	;move char
_	ld a,(charx)
	cp 79
		ret z
	inc a
	ld (charx),a
	ret
	;check to see if return handle to screen
_	ld a,(charx)
	cp 37
		jr nc,---_
	jr --_
char_left:	;left
	ld a,(charT)	;get x
	dec a	;get x-1
	ld d,a
	
	ld a,(charT+1)	;get y
	ld e,a
	
	call Tile_value	;get tile properties of (x-1,y)
	call TileType
		jr z,_	;if solid test position
	
	inc e	;get y+1
	
	call Tile_value	;get tile properties
	call TileType
		jr z,_	;if solid test position
		
	dec e	;get back to y
	ld a,e \ add a,a \ add a,a \ add a,a	;get the offset
	ld e,a	;put it in e
	getY	;get the Y offset of the char
	cp e	;compare with e
		jr z,++_	;if it is equal
	
	ld a,(charT)	;load the x tile loc
	dec a	;get to x-1
	ld d,a	;store to d
	ld a,(charT+1)	;get the y tile loc
	inc a \ inc a	;get to y+2
	ld e,a	;store to e
	call Tile_value	;find out what tile it is
	call TileType	;get the properties
		ret z	;return if solid
	
	jr ++_	;otherwise do normal stuff
	
_	;test position
	inc d	;increase x back to characters tile
	getoffsetD	;get tiles edge
	ld d,a	;store it back to d
	getX	;get the characters edge
	inc d	;increase to one past the edge
	cp d	;compare
		ret c	;if it is less then one past then it is right up against the edge, or it is in the tile
	
_	loadanim(charS,3,4)
	ld a,(mapx)
	or a	;if not at edge of screen
		jr z,++_
	cp 168
		jr z,+++_
	ld a,(charx)
	
	cp 38	;if centered
		jr nz,++_
	;move screen
_	ld a,(mapx)
	or a
		ret z
	dec a
	ld (mapx),a
	ret
	;move char
_	ld a,(charx)
	or a
		ret z
	dec a
	ld (charx),a
	ret
	;check to see if return handle to screen
_	ld a,(charx)
	cp 37
		jr nc,---_
	jr --_
	
;input de=xy
;return a with tile
Tile_value:
	push bc
		push hl
			ld l,e
			ld h,0
			add hl,hl	;*2
			add hl,hl	;*4
			add hl,hl	;*8
			add hl,hl	;*16
			add hl,hl	;*32
			ld c,d
			ld b,0
			add hl,bc
			ld b,h
			ld c,l
			ld hl,MAPUNCRNCH
			add hl,bc
			ld a,(hl)
		pop hl
	pop bc
	ret
	
;input a
;return z for solid
TileType:
	cp 3
		ret z
	cp 4
		ret z
	cp 9
		ret z
	ret

DrawTilemap:
;	Input:		a=x		l=y
;				de=pointer to tilemap
;				ix=pointer to sprites
	push bc
	call	ClearBuff
	ld		h,a
	push	hl
	and		%11111000
	rrca
	rrca
	rrca
	ld		c,a
	ld		a,l
	and		%11111000
	ld		l,a
	xor		a
	ld		b,a
	ld		h,a
	add		hl,hl
	add		hl,hl
	add		hl,bc
	add		hl,de
	pop		de
	ld		a,d
	and		%00000111
	neg
	ld		d,a
	ld		a,e
	and		%00000111
	neg
	ld		e,a
	ld		b,9
__DrawTilemapYLoop
	push	bc
	ld		b,12
__DrawTilemapXLoop
	push	bc
	push	hl
	push	de
	ld		l,(hl)
	ld		h,0
	add		hl,hl
	add		hl,hl
	add		hl,hl
	push	ix
	ex		de,hl
	add		ix,de
	ex		de,hl
	ld		bc,8*256+1
	call	ClipBigSprite
	pop		ix
	pop		hl
	ld		bc,8*256
	add		hl,bc
	ex		de,hl
	pop		hl
	inc		hl
	pop		bc
	djnz	__DrawTilemapXLoop
	ld		a,d
	sub		96
	ld		d,a
	ld		a,e
	add		a,8
	ld		e,a
	ld		bc,20
	add		hl,bc
	pop		bc
	djnz	__DrawTilemapYLoop
	ld		hl,plotSScreen+11
	ld		de,12
	ld		b,64
__DrawTilemapClearColumnLoop
	ld		(hl),d
	add		hl,de
	djnz	__DrawTilemapClearColumnLoop
	pop bc
	ret
