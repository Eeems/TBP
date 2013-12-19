;+----------------------------------------+
;|      Doors CS v7.0 Include File        |
;|         TI-83+/TI-84+ Version          |
;|          Revised July 6, 2010          |
;|           By Kerm Martian              |
;|                                        |
;| Please see http://dcs.cemetech.net for |
;| information and updates including a    |
;| full Developers' SDK.                  |
;+----------------------------------------+

;----> Safe Memory Areas
; saferam1 EQU  768 bytes (APDRAM)
; saferam2 EQU  531 bytes (statRAM)
; saferam3 EQU  128 bytes (textMem)
; saferam4 EQU  66 bytes (OpXs)
; saferam5 EQU  10 bytes (iMathPtrs)

SEram		EQU $086EC
saferam1	EQU 9872h
saferam2	EQU 8A3Ah
saferam3	EQU 8508h
saferam4	EQU 8478h
saferamp	EQU 9872h
saferamp2	EQU 8251h

iversion              EQU  04083h
irandom               EQU  04086h
isprite               EQU  04089h
ilsprite              EQU  0408Ch
igetpix               EQU  0408Fh
ifastcopy             EQU  04092h
idetect               EQU  04095h
idecomp               EQU  04098h

ionversion              EQU  04083h
ionrandom               EQU  04086h
ionputsprite            EQU  04089h
ionlargesprite        EQU  0408Ch
iongetpixel           EQU  0408Fh
ionfastcopy           EQU  04092h
iondetect             EQU  04095h
iondecompress         EQU  04098h

iputsprite            EQU  04089h
ilargesprite        EQU  0408Ch
igetpixel           EQU  0408Fh
idecompress         EQU  04098h

;*************************;
;*  MirageOS Subroutines *;
;*************************;

directin              EQU  0409Bh
sendbytetios          EQU  0409Eh
getbytetios           EQU  040A1h
version               EQU  040A4h
setvputs              EQU  040A7h
setpixel              EQU  040AAh
fastcopys             EQU  040ADh
delayb                EQU  040B0h
multhe                EQU  040B3h
multhl                EQU  040B6h
quittoshell           EQU  040B9h
fastline              EQU  040BCh
pixelonhl             EQU  040BFh
pixeloff              EQU  040C2h
pixelxor              EQU  040C5h
pixeltest             EQU  040C8h
pixeloffhl            EQU  040CBh
pixelxorhl            EQU  040DEh
pixeltesthl           EQU  040D1h
fastlineb             EQU  040D4h
fastlinew             EQU  040D7h
fastlinex             EQU  040DAh
pointonc              EQU  040DDh
pointoffc             EQU  040E0h
pointxorc             EQU  040E3h
centertext            EQU  040E6h
cphlbc                EQU  040E9h
putsprite8            EQU  040ECh
fastcopyb             EQU  040EFh
vputsc                EQU  040F2h
scrolld7              EQU  040F5h
vnewline              EQU  040F8h
rand127               EQU  040FBh
disprle               EQU  040FEh
cphlde                EQU  04101h
screentopic           EQU  04104h
fastlined             EQU  04107h
disprlel              EQU  0410Ah
getnextgoodprog       EQU  0410Dh
getprevgoodprog       EQU  04110h
getnext               EQU  04113h
getprev               EQU  04116h
compstrs              EQU  04119h
nextstr               EQU  0411Ch
getinfo               EQU  0411Fh
fastrectangle         EQU  04122h
gettext               EQU  04125h
gettextv              EQU  04128h
FastRectangle_Save    EQU  0412Bh
vputa                 EQU  0412Eh
runprog               EQU  04131h
isgoodprog            EQU  04134h
existfold             EQU  04137h
delprog               EQU  0413Ah
filledrectangle       EQU  0413Dh
nextfolder            EQU  04140h
delfolder             EQU  04143h
moveall               EQU  04149h
curfoldname           EQU  04149h
curfoldnamea          EQU  0414Ch
createfolder          EQU  0414Fh
compstrsn             EQU  04152h
folder_menu_start     EQU  04155h
options_screen        EQU  04158h
put_folder_name_top   EQU  0415Bh
general_key_routine   EQU  0415Eh
find_num_good_progs   EQU  04161h
put_scrollbar         EQU  04164h
invert_lines          EQU  04167h
invert_1_line         EQU  0416Ah
right_align_value     EQU  0416Dh
put_mirageos_header   EQU  04170h
put_size_graphic      EQU  04173h
sendprog              EQU  04176h
hideprog              EQU  04179h
arcprog               EQU  0417Ch
filledrectangle_save  EQU  0417Fh
getbytetiosw          EQU  04182h
vatswap               EQU  04185h
renameprog            EQU  04188h
renamefolder          EQU  0418Bh
sysmain               EQU  0418Eh
setupint              EQU  04191h
move_gui_prog         EQU  04194h
largespritehl         EQU  04197h
Update_Scrollbar      EQU  0419Ah
Initial_Scrollbar     EQU  0419Dh
sortallfolds          EQU  041A0h
dofoldsort            EQU  041A3h
getfoldsort           EQU  041A6h
setfoldsort           EQU  041A9h
Increase_Cur_Element  EQU  041ACh
Decrease_Cur_Element  EQU  041AFh
Increase_Max_Elements EQU  041B2h
Decrease_Max_Elements EQU  041B5h
Add_A_To_Cur_Element  EQU  041B8h
Sub_A_From_Cur_Element EQU  041BBh
Add_A_To_Max_Elements EQU  041BEh
Sub_A_From_Max_Elements EQU  041C1h
Skip_Forward_B_From_Top EQU  041C4h
Get_Curgoodprog_Ptr   EQU  041C7h
getchecksum           EQU  041CAh
freearc               EQU  041CDh
swapram               EQU  041D0h
hideall               EQU  041D3h
Small_Window 	      EQU  041D6h
ClrDialogFull 	      EQU  041D9h
LargeWindow 	      EQU  041DCh
ClrWinFull 	      EQU  041DFh
PlaySound 	      EQU  041E2h
VDispHL 	      EQU  041E5h
Pause 	              EQU  041E8h
hDetect 	      EQU  041EBh
OpenGUIStack 	      EQU  041EEh
CloseGUIStack	      EQU  041F1h
PushGUIStack 	      EQU  041F4h
PopGUIStack 	      EQU  041F7h
RenderGUI 	      EQU  041FAh
PopGUIStacks	      EQU  041FDh
GUIMouse 	      EQU  04200h
GUIFindFirst 	      EQU  04203h
GUIFindNext 	      EQU  04206h
ResetAppPage 	      EQU  04000h
Cn2_Setup	      EQU  04209h
Cn2_Clear_SendBuf     EQU  0420Ch
Cn2_Clear_RecBuf      EQU  0420Fh
Cn2_Setdown	      EQU  04212h
FileOpen 	      EQU  04215h
FileSave 	      EQU  04218h
FileSaveAs 	      EQU  0421Bh
DispLongInt	      EQU  0421Eh
Cn2_GetK		  EQU  04221h
DPutMap			  EQU  04224h
APGui_gui7ToTop   EQU  04227h
PushGUIStacks	  EQU  0422Ah
GUIFindThis		  EQU  0422Dh

;null				EQU 000
;LargeWin				EQU 001
;SmallWin				EQU 002
;FullScreenImg			EQU 003
;Text					EQU 004
;WinButtons				EQU 005
;WrappedText				EQU 006
;ButtonText				EQU 007
;ButtonImg				EQU 008
;TextLineIn				EQU 009
;Radio				EQU 00A
;Checkbox				EQU 00B
;ByteInt				EQU 00C
;WordInt				EQU 00D
;Hotspot				EQU 00E
;TextMultiline			EQU 00F
;Sprite				EQU 010
;LargeSprite				EQU 011
;PassIn				EQU 012
;ScrollVert				EQU 013
;ScrollHoriz				EQU 014
;Border				EQU 015
;Rect				EQU 016
;Custom				EQU 017
;MouseCursor				EQU 018
GUIRnull					EQU 000h			;COMPLETE	XY	[none]
GUIRLargeWin				EQU 001h			;COMPLETE	--	[5 byte icon, header text]
GUIRSmallWin				EQU 002h			;COMPLETE	XY	[x,y, 5 byte icon, header text]
GUIRFullScreenImg			EQU 003h			;COMPLETE	XY	[768 bytes]
GUIRText					EQU 004h			;COMPLETE	XY	[x,y,font,z.t. text]
GUIRWinButtons				EQU 005h			;COMPLETE	--	[which {-,[],X,00000}, onclick[-] {word}, onclick[] {word}, onclick[X] {word}]
GUIRWrappedText				EQU 006h			;COMPLETE	XY	[x,y,width,font,z.t. text]
GUIRButtonText				EQU 007h			;COMPLETE	XY	[x,y,onclick {word},zt text]
GUIRButtonImg				EQU 008h			;COMPLETE	XY	[x,y,onclick {word},img width [BYTES],button width [PX], img data (5 rows high)]
GUIRTextLineIn				EQU 009h			;COMPLETE	XY	[x,y,width,maxchar [word],curdatapos (0 to datalength) [word],data (z.t.)]
GUIRRadio					EQU 00Ah			;COMPLETE	XY	[x,y,group,state {bool},zttext]  1 per group
GUIRCheckbox				EQU 00Bh			;COMPLETE	XY	[x,y,group,state {bool},zttext] ++ per group
GUIRByteInt					EQU 00Ch			;COMPLETE	XY	[x,y,value,min,max]
GUIRWordInt					EQU 00Dh			;COMPLETE	XY	[x,y,value,min,max]
GUIRHotspot					EQU 00Eh			;COMPLETE	XY	[x,y,width,height,ptr]
GUIRTextMultiline			EQU 00Fh			;COMPLETE	XY	[x,y,rows,width,curdatapos (0 to datalength) [word],data]
GUIRSprite					EQU 010h			;COMPLETE	XY	[x,y,b,data]
GUIRLargeSprite				EQU 011h			;COMPLETE	XY	[x,y,a,b,data]
GUIRPassIn					EQU 012h			;COMPLETE	XY	[x,y,width,maxchar [word],curdatapos (0 to datalength) [word],data (z.t.)]
GUIRScrollVert				EQU 013h			;COMPLETE	XY	[x,y,height,ID,per,min [word],max [word],cur [word],onScrollUp,onScrollDown]
GUIRScrollHoriz				EQU 014h			;COMPLETE	XY	[x,y,width,ID,per,min [word],max [word],cur [word],onScrollUp,onScrollDown]
GUIRBorder					EQU 015h			;COMPLETE	XY	[x,y,width,height,color]
GUIRRect					EQU 016h			;COMPLETE	XY	[x,y,width,height,fill]
GUIRCustom					EQU 017h			;COMPLETE	--	[ptr_to_routine]
GUIRMouseCursor				EQU 018h			;COMPLETE	--  [xy,xy,8-byte mask,8-byte sprite]
GUIRMouseClick				EQU 019h		

;other compatibility equates

;End of file

;end