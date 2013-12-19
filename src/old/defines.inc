;small routines
#DEFINE dialog(text_pointer) ld hl,text_pointer \ call Text_Output
#DEFINE wrappedtext(text_pointer) ld hl,text_pointer \ call DrawWrappedString
#DEFINE monospacetext(text_pointer) ld hl,text_pointer \ call DrawMonospaceString
#DEFINE picture(img_pointer,height,start_at) LD HL, img_pointer \ LD DE, PlotSScreen+(start_at*12) \ LD BC, height*12 \ LDIR
#DEFINE largeSprite(Spritex,Spritey,Spritew,Spriteh,pointer) ld l,Spritey \ ld a,Spritex \ ld b,Spriteh \ ld c,Spritew \ ld ix,pointer \ call ILargeSprite
#DEFINE largeSpriteAND(Spritex,Spritey,Spritew,Spriteh,pointer) ld l,Spritey \ ld a,Spritex \ ld b,Spriteh \ ld c,Spritew \ ld ix,pointer \ call ALargeSprite
#DEFINE maskedSprite(Spritex,Spritey,pointer) ld d,Spritex \ ld e,Spritey  \ ld hl,pointer \ call putmaskedSprite
#DEFINE sprite(Spritex,Spritey,Spriteh,pointer) ld a,Spritex \ ld l,Spritey \ ld b,Spriteh \ ld ix,pointer \ call IPutSprite
#DEFINE menu(tablepointer) ld hl,tablepointer \ jp MenuRoutine
#DEFINE rectangle(R1x,R1y,R2x,R2y,color) ld h,R1x \ ld l,R1y \ ld d,R2x \ ld e,R2y \ ld a,color \ call DrawCustomRectangle
#DEFINE box(R1x,R1y,R2x,R2y,color) ld h,R1x \ ld l,R1y \ ld d,R2x \ ld e,R2y \ ld a,color \ call Fastrectangle
#DEFINE tilemap(mappointer,spritespointer,Tilemapx,Tilemapy) ld a,Tilemapx \ ld l,Tilemapy \ ld ix,spritespointer \ ld de,mappointer \ call DrawTilemap
#DEFINE dispPause call Fastcopys \ call Pause
#DEFINE homeup push af \ xor a \ ld (penCol),a \ ld (penRow),a \ pop af
#DEFINE loadanim(anim_pointer,Sprite1,Sprite2) ld a,Sprite1 \ ld (anim_pointer+3),a \ ld a,Sprite2 \ ld (anim_pointer+4),a
#DEFINE dispA(dispx,dispy,pointer) ld a,dispx \ ld (penCol),a \ ld a,dispy \ ld (penRow),a \ ld a,(pointer) \ call VDispA
#DEFINE dispHL(dispx,dispy,pointer) ld a,dispx \ ld (penCol),a \ ld a,dispy \ ld (penRow),a \ ld hl,(pointer) \ call VDispHL
#DEFINE GPO(top,obot,nbot) ld a,(top) \ ld h,a \ ld a,(obot) \ ld c,a \ ld e,nbot \ call battle_get_percent
#DEFINE GPOG(top,obot,nbot) ld a,(top) \ ld h,a \ ld a,(obot) \ ld c,a \ ld e,nbot \ call battle_set_graph
#DEFINE Disp(dispx,dispy,value) ld a,dispx \ ld (penCol),a \ ld a,dispy \ ld (penRow),a \ ld a,(value) \ ld l,a \ ld h,0 \ call VDispHL
#DEFINE decompress(pointer)	ld de,MAPUNCRNCH \ ld hl,pointer \ call Uncrunch

;easier reding
#DEFINE nextLine call Newline
#DEFINE AtoHL ld l,a \ ld h,0
#DEFINE lcdupdate call Fastcopys
#DEFINE clearlcd call ClearBuff
#DEFINE lcdclearall call ClearFull
#DEFINE getkey(KEY) ld bc,KEY \ call dkey
#DEFINE getsprite(id,sprite) ld a,id \ ld hl,sprite \ call game_get_sprite 
#DEFINE storeR	push hl \ push de \ push bc \ push af
#DEFINE recallR pop af \ pop bc \ pop de \ pop hl
#DEFINE getX ld a,(mapx) \ ld ix,charx \ add a,(ix+0)
#DEFINE getY ld a,(mapy) \ ld ix,chary \ add a,(ix+0)
#DEFINE gettileY ld a,(mapy) \ ld ix,chary \ add a,(ix+0) \ srl a \ srl a \ srl a
#DEFINE gettileX ld a,(mapx) \ ld ix,charx \ add a,(ix+0) \ srl a \ srl a \ srl a
#DEFINE gettileA srl a \ srl a \ srl a
#DEFINE getoffset(offset) ld a,(offset) \ sra a \ sra a \ sra a
#DEFINE getoffsetD ld a,d \ add a,a \ add a,a \ add a,a
#DEFINE getoffsetE ld a,e \ add a,a \ add a,a \ add a,a
#DEFINE getoffsetA  add a,a \ add a,a \ add a,a
#DEFINE gettile(tilex,tiley) ld d,tilex \ ld e,tiley \ call Tile_value
#DEFINE parse_scene(pointer) ld hl,pointer-1 \ call parse_cutscene
#DEFINE copy(cpy_from,cpy_to,cpy_len) ld hl,cpy_from \ ld de,cpy_to \ ld bc,cpy_len \ ldir

;value replacement
gbuf			equ		PlotsScreen	;768 bytes
Text_buffer		equ		saferam2	;531 bytes
MAPUNCRNCH		equ		$8000		;1024 bytes
#DEFINE			dw		.dw			;
ProgStart		equ		$9D95		;

;settings
;#DEFINE DEBUG_ENABLED

;------>	Location defines
tmpbuf			equ saferam4			;2 byte
_tmpbuf			equ saferam4	;copy
initstack		equ saferam3			;2 byte
CurrentContrast	equ	initstack+2			;2 byte
charT			equ	CurrentContrast+2	;3 byte
charJ			equ	charT+3				;2 byte
_cursor			equ	charJ+2				;1 byte
;------>	Savable stats/vars
SaveStart		equ	_cursor+1			;0 byte
gameMode		equ SaveStart			;1 byte
LCutscene		equ gameMode+1			;2 byte
level			equ	LCutscene+2			;1 byte
mapx			equ	level+1				;1 byte
mapy			equ	mapx+1				;1 byte
mapD			equ	mapy+1				;4 byte
charx			equ	mapd+4				;1 byte
chary			equ	charx+1				;1 byte
charS			equ	chary+1				;7 byte
Name			equ	charS+7				;12 byte
BlackMageStats	equ	Name+12				;18 bytes
EnemyStats		equ BlackMageStats+18	;18 bytes
EnemyType		equ EnemyStats+18		;1 byte	(0=no run 1+=chances)
EnemyName		equ EnemyType+1			;12 bytes
;------>	Offsets
jump			equ	0					;jump height
hover			equ	1					;hover length
health			equ	2					;current heath
mhealth			equ	3					;max health
mana			equ	4					;current mana
mmana			equ	5					;max mana
strength		equ	6					;attk modifier
resistance		equ	7					;defence modifier
agility			equ	8					;dodge modifier
black			equ	9					;<----
white			equ	10					;	|
grey			equ	11					;	|
brown			equ	12					;	|
green			equ	13					; Magics
yellow			equ	14					;	|
purple			equ	15					;	|
red				equ	16					;	|
blue			equ	17					;<---

;Uncrunch
escPu         = tmpbuf+2
OutPtr        = escPu+1
lzpos         = OutPtr+2
EscBits       = lzpos+2
Esc8Bits      = EscBits+1
MaxGamma      = Esc8Bits+1
Max1Gamma     = MaxGamma+1
Max2Gamma     = Max1Gamma+1
Max8Gamma     = Max2Gamma+1
ExtraBits     = Max8Gamma+1
tablePu       = ExtraBits+1
regy          = tablePu+31

;DEBUG DEFINES
#DEFINE debug di \ halt
#DEFINE debugHLP push hl \ push de \ push af \ ld a,(hl) \ ld h,0 \ ld l,a \ b_call _dispHL \ call Pause \ pop af \ pop de \ pop hl
#DEFINE debugA push hl \ push de \ push af  \ call VdispA \ call Fastcopys \ call Pause \ pop af \ pop de \ pop hl
#DEFINE debugAh push hl \ push de \ push af  \ call ClearFull \ ld l,a \ ld h,0 \ b_call _DispHL \ call Pause \ pop af \ pop de \ pop hl
#DEFINE debugL push hl \ push de \ push af  \ ld a,l \ call VdispA \ call Fastcopys \ call Pause \ pop af \ pop de \ pop hl
#DEFINE debugHL push hl \ push de \ push af  \ push bc \ call VdispHL \ call Fastcopys \ call Pause \ pop bc \ pop af \ pop de \ pop hl
#DEFINE debugAP push hl \ push de \ push af  \ b_call _VPutMap \ call Fastcopys \ call Pause \ pop af \ pop de \ pop hl