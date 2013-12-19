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
Init:                          	;main routines
	set TextWrite,(iy+SGrFlags)
	res DonePrgm,(iy+DoneFlags)
	bcall _runIndicOff	;turn off the run indicator, if I can read correctly :P
	xor a
	out ($20),a	;set to 6MHz mode
	homeup	;set penCol/Row to 0
	ld a,(contrast)
	ld (CurrentContrast),a	;get and store the contrast for later
Start:
	clearlcd	;what it says
	decompress(TitleImg)	;decompress the title image
	picture(MAPUNCRNCH,64,0)	;display it
	ld (initstack),sp	;store the initial stack position for later
	menu(MainMenu)	;run the main menu
New:
	ld de,SaveStart
	ld hl,DefaultSettings
	ld bc,DefaultSettingsEnd-DefaultSettings
	ldir
	jr Run
Load:
	ld hl,save_appvar_name
	rst 20h
	b_call _chkfindsym
		jr c,New
	xor a
	cp b
		jr z,_
	ld hl,save_appvar_name
	rst 20h
	b_call _Arc_Unarc
_	ld hl,2 ;+2 is for the size word
	add hl,de	;get the offset into hl
	ld bc,DefaultSettingsEnd-DefaultSettings
	ld de,SaveStart
	ldir
Run:
	ld a,(gameMode)
	or a
		jr z,++_
	cp 1
		jr z,+_
	ld hl,(LCutscene)
	dec hl
	call parse_cutscene
_	call battle_engine
_	ld a,(level)
	call Game_engine
	jr Start	;return to menu
End:
	decompress(TitleImg)
	picture(MAPUNCRNCH,64,0)
	lcdupdate
	b_call 5011h	;_FillBasePageTable
	ret
NameSettings:
	call name_menu
	jr Settings
ContrastSettings:
	call contrast_menu
Settings:
	menu(SettingsMainMenu)
About:
	dialog(AboutMsg)
	jp Start
#include "parse.asm"
#include "platform_engine.asm"
#include "battle_engine.asm"
#include "Routines.asm"
#include "uncrunch.asm"
#include "math.asm"
#include "keys.asm"
#include "text.asmdat"
#include "tiles.asmdat"
#include "char_blackmage.asmdat"
#include "char_civilian.asmdat"
#include "char_monster.asmdat"
#include "maps.asmdat"
#include "menus.asmdat"
#include "cutscenes.asmdat"
#include "data.asmdat"
#include "img.asmdat"
