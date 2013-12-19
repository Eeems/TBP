battle_engine:
	ld a,1	;switch to battle mode
	ld (gameMode),a
	call battle_drawscreen
	copy(battle_intro,text_buffer,battle_intro_end-battle_intro)
	ld hl,EnemyName
	ld bc,12
	ldir
	dialog(text_buffer)
battle_return:
	call battle_drawscreen
	rectangle(29,23,62,47,0)
	rectangle(30,22,61,48,0)
	largesprite(29,22,4,25,battle_actions)
	lcdupdate
	call Pause
	push af	;preserve key return
		call battle_drawscreen
	pop af
	cp skUp
		jr z,battle_char_attk
	cp skDown
		jp z,battle_run
	cp skLeft
		jp z,battle_magic
	cp skRight
;		jr z,battle_items
	cp skClear
		jp z,battle_menu
	cp skGraph
		jp z,battle_menu
	cp skMode
		jp z,battle_menu
	jr battle_return
battle_finish:
	clearlcd
	ret
battle_char_attk:
	ld a,(EnemyStats+agility)
	ld c,a
	srl c
	srl c	;Enemy_agility/4
	ld hl,(BlackMageStats+agility)
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl	;Char_agility*32
	call HLdC	;char_result/enemy_result = F
	ld a,h
	or a
		jp nz,battle_dodge	;automatic dodge if F is over 255
	ld b,255
	call iRandom	;random number between 0-255 in a
	cp l	;compare to F (l)
		jp c,battle_dodge	;if less then F dodge
	ld a,(EnemyStats+resistance)
	ld b,a
	ld a,(BlackMageStats+strength)
_		or a
			jr z,$+3
		dec a
		djnz -_	;a contains char_strength-enemy_resistance or 0 if it is lower
	ld c,a
	ld a,(BlackMageStats+strength)
	ld b,a
	srl b	;get half of char_strength
_		dec a	;do char_strength-(char_strenght/2)
		djnz -_
	ld b,a	;contains char_strength-(char_strenght/2)
	call iRandom
	add a,c	;a contains amount of damage to do
	ld b,a
	ld a,(EnemyStats+health)
_		or a
			jr z,$+3
		dec a
		djnz -_	;a contains enemy_health-damage or 0 if it is lower
	ld d,a
	ld a,(EnemyStats+mana)
	ld e,a
	ld a,(BlackMageStats+health)
	ld h,a
	ld a,(BlackMageStats+mana)
	ld l,a
	push hl
		push de
			push bc
			call tobackbuff
			largesprite(58,0,2,32,battle_enemy_damage)
			lcdupdate
			call Slowdown
			call frombackbuff
			largesprite(59,1,2,32,battle_enemy_damage)
			lcdupdate
			call Slowdown
			call frombackbuff
			largesprite(60,2,2,32,battle_enemy_damage)
			lcdupdate
			call Slowdown
			call frombackbuff
			largesprite(61,3,2,32,battle_enemy_damage)
			lcdupdate
			call Slowdown
			call frombackbuff
			largesprite(62,4,2,32,battle_enemy_damage)
			lcdupdate
			call Slowdown
			call frombackbuff
			pop bc
		pop de
	pop hl
	call battle_change_values
	jp battle_return
battle_enemy_attk:
	ld a,(BlackMageStats+agility)
	ld c,a
	srl c
	srl c	;Char_agility/4
	ld hl,(EnemyStats+agility)
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl	;enemy_agility*32
	call HLdC	;enemy_result/char_result = F
	ld a,h
	or a
		jr nz,battle_dodge	;automatic dodge if F is over 255
	ld b,255
	call iRandom	;random number between 0-255 in a
	cp l	;compare to F (l)
		jr c,battle_dodge	;if less then F dodge
	ld a,(BlackMageStats+resistance)
	ld b,a
	ld a,(EnemyStats+strength)
_		or a
			jr z,$+3
		dec a
		djnz -_	;a contains enemy_strength-char_resistance or 0 if it is lower
	ld c,a
	ld a,(EnemyStats+strength)
	ld b,a
	srl b	;get half of enemy_strength
_		dec a	;do enemy_strength-(enemy_strenght/2)
		djnz -_
	ld b,a	;contains enemy_strength-(enemy_strenght/2)
	call iRandom
	add a,c	;a contains amount of damage to do
	ld b,a
	ld a,(BlackMageStats+health)
_		or a
			jr z,$+3
		dec a
		djnz -_	;a contains char_health-damage or 0 if it is lower
	ld d,a
	ld a,(BlackMageStats+mana)
	ld e,a
	ld a,(EnemyStats+health)
	ld h,a
	ld a,(EnemyStats+mana)
	ld l,a
	call battle_change_values
	jp battle_return
battle_dodge:
	homeup
	dialog(battle_missed)
	jp battle_return
battle_run:
	ld a,(EnemyType)
	or a
		jp z, battle_return
	ld b,a
	call iRandom
	or a
		jr nz,_
	homeup
	dialog(battle_run_escape)
	jp battle_finish
_	homeup
	dialog(battle_run_fail)
	jp battle_return
battle_quit:
	ld sp,(initstack)
	jp Start
battle_contrast:
	call contrast_menu
	jr battle_settings
battle_name:
	call name_menu
battle_settings:
	menu(BattleSettingsMenu)
battle_menu:
	menu(BattleMenu)
battle_drawscreen:
	decompress(Battle_Screen)
	picture(MAPUNCRNCH,64,0)
	sprite(6,6,9,battle_enemy_face)
	decompress(battle_enemy)
	largesprite(60,3,2,32,MAPUNCRNCH)
	decompress(battle_blackmage_back)
	largesprite(20,15,3,49,MAPUNCRNCH)
	Disp(27,5,EnemyStats+health)
	Disp(58,52,BlackMageStats+health)
	;draw user health
	GPOG(BlackMageStats+health,BlackMageStats+mhealth,100)
	ld a,l
	push af
		GPO(BlackMageStats+health,BlackMageStats+mhealth,47)
		ld b,96
		call BsL	;l = 96-l
		ld h,l
		ld l,60
		ld d,95
		ld e,63
	pop af
	call DrawCustomRectangle
	;draw user mana
	GPOG(BlackMageStats+mana,BlackMageStats+mmana,100)
	ld a,l
	push af
		GPO(BlackMageStats+mana,BlackMageStats+mmana,47)
		ld b,64
		call BsL	;l = 64-l
		ld h,92
		ld d,95
		ld e,63
	pop af
	call DrawCustomRectangle
	;draw enemy health
	GPOG(EnemyStats+health,EnemyStats+mhealth,100)
	ld a,l
	push af
		GPO(EnemyStats+health,EnemyStats+mhealth,47)
		ld d,l
		ld h,1
		ld l,1
		ld e,4
	pop af
	call DrawCustomRectangle
	;draw enemy mana
	GPOG(EnemyStats+mana,EnemyStats+mmana,100)
	ld a,l
	push af
		GPO(EnemyStats+mana,EnemyStats+mmana,47)
		ld e,l
		ld h,1
		ld l,1
		ld d,4
	pop af
	call DrawCustomRectangle
	lcdupdate
	ret
battle_set_graph:
	call battle_get_percent
	ld a,50
	cp l
		jr c,_
	ld l,85	;checkered
	ret
_	ld l,$FF	;black
	ret
;inputs: hl=new_char(health,mana) de=new_enemy(health,mana)
;description: animation to change health/mana bars
battle_change_values:
	ld a,(BlackMageStats+health)
	ld b,a	;get old value
	ld c,h	;get new value
	call battle_check_values	;change values
	ld (BlackMageStats+health),a	;store it
	ld a,(BlackMageStats+mana)
	ld b,a	;get old value
	ld c,l	;get new value
	call battle_check_values	;change values
	ld (BlackMageStats+mana),a	;store it
	ld a,(EnemyStats+health)
	ld b,a	;get old value
	ld c,d	;get new value
	call battle_check_values	;change values
	ld (EnemyStats+health),a	;store it
	ld a,(EnemyStats+mana)
	ld b,a	;get old value
	ld c,e	;get new value
	call battle_check_values	;change values
	ld (EnemyStats+mana),a	;store it
	push hl
		push de
			call Slowdown
			call battle_drawscreen
		pop de
	pop hl
	ld a,(BlackMageStats+health)
	cp h
		jr nz,battle_change_values
	ld a,(BlackMageStats+mana)
	cp l
		jr nz,battle_change_values
	ld a,(EnemyStats+health)
	cp d
		jr nz,battle_change_values
	ld a,(EnemyStats+mana)
	cp e
		jr nz,battle_change_values
	ret
;inputs: bc=(old,new)
;outputs: a=incremental value to new value			nc=(old>=new) c=(old<new)
battle_check_values:
	ld a,c
	cp b
		ret z	;if equal
		jr nc,_	;if b>=c
	ld a,b
	dec a
	ret
_	ld a,b
	inc a
	ret
;inputs: h=top c=old_bottom e=new_bottom
;outputs: new value in l (if big enough hl)
battle_get_percent:
	push bc
		call HmE	;hl = top*new_bottom
	pop bc
	call HLdC	;hl = hl/old_bottom
	ret
;displays the magic menu
battle_magic:
	call tobackbuff
	rectangle(5,4,75,61,0) \ rectangle(4,5,76,60,0)
	largeSprite(4,4,9,57,MenuImg)
	ld hl,(6*256)+6
	ld (penCol),hl
	wrappedtext(MagicMenuTitle)
	ld (_tmpbuf),a
	ld b,a
	ld hl,Magic_strings
	ld de,BlackMageStats+black	;thanks Runner112 no idea what some of this does
	;;WHILE spells remain
_		ld a,(de)
		or a
		jr z,+_
		;;IF spell is available
			push de
				ld de,_tmpbuf
				ld a,(de)
				inc a
				ld (de),a
				ld a,b
				inc b
				call battle_magic_menu_cursorpos
				add a,10
				ld e,a
				inc d
				ld (pencol),de
				call DrawWrappedString
			pop de
		jr ++_
		;;ELSE
_			ld c,d
			cpir
		;;END
_		inc de
		ld a,e
		sub BlackMageStats+blue+1 & $FF
	jr nz,---_
	;;ENDIF no spells remain
	ld a,(_tmpbuf)
	or a
	jr nz,_
	;;IF no spells are available
	push af
		call Slowdown
		call Slowdown	;slowdown so it's humanly possible to release the button in time so the dialog doesn't dissapear.
	pop af
	dialog(error_none)
	
		call frombackbuff
		jp battle_return
	;;END
_	xor a
	push af
battle_magic_loop:
	;;WHILE waiting for a user input
		call battle_magic_menupick	;draw pointer
		lcdupdate
		call battle_magic_menupick	;erase pointer
		b_call _getCSC
		dec a				;effectively sub skDown
		jr z,battle_magic_down		;;IF down was pressed, GOTO down handler
		sub skUp-skDown
		jr z,battle_magic_up		;;IF up was pressed, GOTO up handler
		sub sk2nd-skUp
		jr z,battle_magic_handler	;;IF 2nd was pressed, GOTO magic handler
		sub skEnter-sk2nd
		jr z,battle_magic_handler	;;IF enter was pressed, GOTO magic handler
		sub skAlpha-skEnter
		jr z,_				;;IF alpha was pressed, GOTO end of loop
		sub skClear-skAlpha
	jr nz,battle_magic_loop
	;;ENDIF clear was pressed
_	pop af
	call frombackbuff
	jp battle_return
battle_magic_down:
	pop af
	ld hl,_tmpbuf
	inc a
	cp (hl)
_	adc a,-1
	push af
	jr battle_magic_loop
battle_magic_up:
	pop af
	cp 1
	jr -_
battle_magic_menupick:
	pop hl
	pop af
	push af
	push hl
	call battle_magic_menu_cursorpos
	ld l,d
	ld b,8
	ld ix,menupicker
	jp IPutSprite
battle_magic_menu_cursorpos:
	ld de,(7*256)+6
	cp d
	jr c,_
	;;IF item is in second column
		sub d
		ld e,48
	;;END
_	ld d,a
	add a,a
	add a,d
	add a,a
	add a,16
	ld d,a
	ld a,e
	ret
battle_magic_handler:
	pop af
	dec a
	;;Check which magic here
	push af
		call Slowdown
		call Slowdown	;slowdown so it's humanly possible to release the button in time so the dialog doesn't dissapear.
	pop af
	dialog(error_null)
	
	call frombackbuff
	jp battle_return
;---------------------------------------------story---------------------------------------------------------;
; <shmibs>	earlier, when i got off work, i was feeling a bit like shit, right?								;
;	so i went over to the grass to lay down for a bit														;
;	(this was around 8:30 at night)																			;
; <Netham45>	long story short, you woke up really confused with coke on your nose in Cuba?				;
; * Netham45	shuts up.																					;
; <shmibs>	and a drunkard stumbled over from one of the nearby bars and pissed on my stomach				;
;	 =D																										;
; <geekboy>	...																								;
;	 thats just wrong																						;
; <Netham45>	I'd rather wake up in Cuba with coke on my nose												;
; <geekboy>	^																								;
; <shmibs>	oh, but here's the best part:																	;
;	i shouted at him, and he screamed like a little girl and hopped away while trying to zip up his pants	;
; <Netham45>	heh																							;
; <shmibs>	anyways, i'm feeling a little grossed out														;
;	 how is everyone?!																						;
;-----------------------------------------------------------------------------------------------------------;