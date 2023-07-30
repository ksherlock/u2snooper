; u2 snooper CDA

; assumes slot 3.
; snasm /l /z /m/ w /g u2.asm,u2.obj,,u2.lst


	opt ws- ; relaxed white space
	opt od+ ; direct-page optimization

IDM_OR	equ $e0c0b0
IDM_AR	equ $e0c0b1
IDM_DR	equ $e0c0b3

KEY_LEFT	equ $08
KEY_RIGHT	equ $15
KEY_UP		equ $0b
KEY_DOWN	equ $0a
KEY_ESC		equ $1b	


; direct page
; rw = reserve word
; uses the __rs variable.
	rsreset
page	rw 1 ; current active page.

ptr	rw 1
str_ptr	rw 1 

bcd	rw 2
tmp	rw 1
scratch	rw 2 ; rl not supported...
addr	rw 2

;
_SysBeep macro
	ldx #$2c03
	jsl $e10000
	endm

_beep	macro
	local mmxx
mmxx	= __mx
	if mmxx<>0
	php
	rep #$30
	endif
	_SysBeep
	if mmxx<>0
	plp
	mx mmxx
	endif
	endm



pstr	macro
	local .start,.end
	db .end-.start ; n.b. - macro expansion sometimes displays the wrong value
.start
	db \1
.end
	endm

gsstr	macro
	local .start,.end
	dw .end-.start ; n.b. - macro expansion sometimes displays the wrong value
.start
	db \1
.end
	endm


__set_bits	macro
	if	'\1'='m'
bits	=	bits|%10
	endif
	if	'\1'='M'
bits	=	bits|%10
	endif
	if	'\1'='x'
bits	=	bits|%01
	endif
	if	'\1'='X'
bits	=	bits|%01
	endif

	if	'\1'='mx'
bits	=	bits|%11
	endif
	if	'\1'='MX'
bits	=	bits|%11
	endif


	endm

;	short m,x
short	macro
	local bits
bits	= 0
	if narg=0
bits	= %11
	else
		rept narg
			__set_bits \1
			shift
		endr
	endif

	sep #bits<<4
; __mx is auto-set by sep/rep
;	mx __mx|bits
	endm

long	macro
	local bits
bits = 0
	if narg=0
bits = %11
	else
		rept narg
			__set_bits \1
			shift
		endr
	endif

	rep #bits<<4
;	mx __mx&~bits
	endm



	section code

	pstr "Uthernet II Snooper"
	dl StartUp
	dl ShutDown


ShutDown
	mx %00

	rtl

StartUp
	mx %00

	phk
	plb

	lda >IDM_OR
	sta saved_or
	lda >IDM_AR
	sta saved_ar

	lda #$03 ;
	short m,x
	sta >IDM_OR

	jsr screen_init

	stz page
;	jsr print_footer
	jsr display_common

mainloop

@wait	lda >$e0c000
	bpl @wait
	sta >$e0c010


	and #$7f
	cmp #'a'
	blt @upper
	cmp #'z'+1
	bge @upper
	and #~$20
@upper
	cmp #$1b ; esc
	beq quit
	cmp #'Q'
	beq quit
	ldx #@ksize-2
@loop
	cmp @k,x
	bne @next
	jsr (dispatch,x)
	bra mainloop
@next
	dex
	dex
	bpl @loop
	bra @wait 


;
; todo:
; i -> clear interrupts

PAGE_COMMON equ 0
PAGE_0	equ 1
PAGE_1	equ 2
PAGE_2	equ 3
PAGE_3	equ 4
PAGE_TX equ 5
PAGE_RX	equ 6
PAGE_HELP equ 7

@k	dw 'C','0','1','2','3','T','R','?',':',KEY_LEFT,KEY_RIGHT,KEY_DOWN,KEY_UP
@ksize	equ *-@k


quit
	long m,x
	lda saved_or
	sta >IDM_OR
	lda saved_ar
	sta >IDM_AR

	rtl
	mx %11



dispatch
	dw display_common,display_0,display_1,display_2,display_3,display_tx,display_rx,display_help
	dw set_memory,@left,@right,@down,@up


@left
	lda page
	dec
	bpl @d
	lda #PAGE_RX
@d
	sta page
	asl
	tax
	jmp (dispatch,x)

@right
	lda page
	inc
	cmp #PAGE_RX+1
	bcc @d
	lda #0
	bra @d

; auto-increment wraps at $2000 so nothing special needed.
@down
	lda page
	cmp #PAGE_RX
	beq @down.ok
	cmp #PAGE_TX
	beq @down.ok
	rts
@down.ok
	jmp dump_common

@up
	lda page
	cmp #PAGE_RX
	beq @up.ok
	cmp #PAGE_TX
	beq @up.ok
	rts
@up.ok
	; $40 -> $5f; 60 -> $7f
	lda >IDM_AR
	bit #$1f
	bne @up.dec

	ora #$1f
	sta >IDM_AR
	jmp dump_common

@up.dec
	dec
	sta >IDM_AR
	jmp dump_common




rdispatch
	dw display_common,display_0,display_1,display_2,display_3,dump_common,dump_common,display_help


refresh
	lda page
	asl
	tax
	jmp (rdispatch,x)



display_help
	mx %11

	lda #PAGE_HELP
	sta page

	jsr screen_clear

	lda #@title
	sta str_ptr
	lda #@title>>8
	sta str_ptr+1
	jsr print_str
	jsr print_cr
	inc screen_y
	inc screen_y

	ldx #0
@loop
	lda @table,x
	inx
	sta str_ptr
	lda @table,x
	inx
	sta str_ptr+1
	phx
	jsr print_str
	jsr print_cr
	plx
	cpx #18*2
	bcc @loop
	rts


@table
	dw @00,@01,@02,@03,@04,@05,@06,@07,@08,@09
	dw @10,@11,@12,@13,@14,@15,@16,@17,@18,@19

@title	db "Help",0

@00	db "C - Common Registers",0
@01	db "0 - Socket 0 Registers",0
@02	db "1 - Socket 1 Registers",0
@03	db "2 - Socket 2 Registers",0
@04	db "3 - Socket 3 Registers",0
@05	db "R - Receive Buffer",0
@06	db "T - Transmit Buffer",0
@07	db ": - Write memory",0
@08	db "? - Help",0
@09	db "",0
@10	db "Socket Commands:",0
@11	db "$01: Open",0
@12	db "$02: Listen",0
@13	db "$04: Connect",0
@14	db "$08: Disconnect",0
@15	db "$10: Close",0
@16	db "$20: Send",0
@17	db "$21: Send Mac",0
@18	db "$22: Send Keep alive",0
@19	db "$40: Receive",0




;print_footer
;	mx %11
;	stz screen_x
;	lda #23*2
;	sta screen_y

;	pea @help
;	pla
;	sta str_ptr
;	pla
;	sta str_ptr+1
;	jmp print_str
;@help	db "Q)uit C)ommon 0 1 2 3",0

print_register	module
	mx %11

	lda (ptr)

	sta >IDM_AR+1

	jsr print_hex
	inc screen_x
	ldy #1

; print the label
@loop
	lda (ptr),y
	bmi load
	phy
	jsr print
	ply
	iny
	bra @loop

; read the data..
load
	phy ; save
	and #$7f
	asl
	tax ; byte count / type
	jsr (table,x)
	ply
	iny
	lda (ptr),y
	tax
	jmp (extra,x)

table
	dw @rts
	dw byte1
	dw byte2
	dw @rts
	dw byte4
	dw @rts
	dw byte6


XTRA_NOP	equ 0
XTRA_DEC	equ 2
XTRA_SN_SR	equ 4
XTRA_SN_IR	equ 6
XTRA_SN_MR	equ 8
XTRA_IR		equ 10

extra	dw print_cr
	dw do_xtra_dec
	dw do_xtra_sn_sr
	dw do_xtra_sn_ir
	dw do_xtra_sn_mr
	dw do_xtra_ir

@rts
	rts
	modend

byte1
	stz scratch+1 ; so XTRA_DEC will work
	lda >IDM_DR
	sta scratch
	jmp print_hex

byte2
	lda >IDM_DR
	sta scratch+1
	jsr print_hex
	lda >IDM_DR
	sta scratch
	jmp print_hex

byte4	; ip address...

	rept 3
	lda >IDM_DR
	jsr print_dec
	lda #'.'
	jsr print
	endr

	lda >IDM_DR
	jmp print_dec

byte6	; mac address

	rept 5
	lda >IDM_DR
	jsr print_hex
	lda #':'
	jsr print
	endr

	lda >IDM_DR
	jmp print_hex

do_xtra_dec
	lda #' '
	jsr print
	lda #'('
	jsr print
	ldx scratch
	ldy scratch+1
	jsr print_dec_16
	lda #')'
	jsr print
	jmp print_cr

do_xtra_sn_sr	module
	; print the socket status
	lda #' '
	jsr print

	ldx #@srsize-2
	lda scratch
@loop	cmp @sr,x
	beq @match
	dex
	dex
	bpl @loop
	jmp print_cr
@match
	lda @ltable,x
	sta str_ptr
	lda @ltable+1,x
	sta str_ptr+1
	jsr print_str
	jmp print_cr

@sr	dw $00,$13,$14,$17,$1c,$22,$32,$42,$5f,$15,$16,$18,$1a,$1b,$1d,$01
@srsize	equ *-@sr

@ltable
	dw @l00,@l13,@l14,@l17,@l1c,@l22,@l32,@l42,@l5f,@l15,@l16,@l18,@l1a,@l1b,@l1d,@l01

@l00	db 'closed',0
@l13	db 'init',0
@l14	db 'listen',0
@l17	db 'established',0
@l1c	db 'close wait',0
@l22	db 'udp',0
@l32	db 'ipraw',0
@l42	db 'macraw',0
@l5f	db 'pppoe',0
@l15	db 'syn sent',0
@l16	db 'syn recv',0
@l18	db 'fin wait',0
@l1a	db 'closing',0
@l1b	db 'time wait',0
@l1d	db 'last ack',0
@l01	db 'arp',0
	modend

do_xtra_sn_mr	module
	mx %11

	lda #' '
	jsr print

	lda #@t>>8
	sta str_ptr+1

	lda scratch
	and #$0f
	cmp #6
	bcc @ok
	lda #6

@ok
	asl ; x 2
	asl ; x 4
	; must be clc since <= 6*4
	adc #@t
	sta str_ptr
	jsr print_str

@bit7
	; only applies to udp
	bit scratch
	bpl @bit6
	lda #@multi
	sta str_ptr
	lda #@multi>>8
	sta str_ptr+1
	jsr print_str

@bit6
	; only applies to macraw / socket 0.
	bit scratch
	bvc @bit5
	lda #@mf
	sta str_ptr
	lda #@mf>>8
	sta str_ptr+1
	jsr print_str

@bit5
	; nd/mc depend on tcp/udp status.
	lda scratch
	and #%00100111
	cmp #%00100001
	beq @pnd
	cmp #%00100010
	beq @pmc
	jmp print_cr
@pnd
	lda #@nd
	sta str_ptr
	lda #@nd>>8
	sta str_ptr+1
	jsr print_str	
	jmp print_cr

@pmc
	lda #@mc
	sta str_ptr
	lda #@mc>>8
	sta str_ptr+1
	jsr print_str	
	jmp print_cr

@t
	db '---',0
	db 'tcp',0
	db 'udp',0
	db 'ip ',0
	db 'mac',0
	db 'ppp',0
	db '???',0
@multi	db ', multi',0
@mf	db ', mf',0
@nd	db ', nd',0
@mc	db ', mc',0



	modend


do_xtra_sn_ir	module
	mx %11

	lda #'-'
	ldx #7
@loop1	sta @str,x
	dex
	bpl @loop1


	lda scratch
	ldx #7

@loop2
	lsr
	bcc @next

	phx
	pha
	txa
	lda @t,x
	sta @str,x
	pla
	plx

@next	dex
	bpl @loop2

	lda #' '
	jsr print

	pea @str
	pla
	sta str_ptr
	pla
	sta str_ptr+1

	jsr print_str
	jmp print_cr

@t	db '???STRDC'
@str	db '--------',0

	modend

do_xtra_ir	module
	mx %11

	lda #'-'
	ldx #7
@loop1	sta @str,x
	dex
	bpl @loop1


	lda scratch
	ldx #7

@loop2
	lsr
	bcc @next

	phx
	pha
	txa
	lda @t,x
	sta @str,x
	pla
	plx

@next	dex
	bpl @loop2

	lda #' '
	jsr print

	pea @str
	pla
	sta str_ptr
	pla
	sta str_ptr+1

	jsr print_str
	jmp print_cr

@t	db 'CUP?3210'
@str	db '--------',0

	modend



display_common	module
	mx %11

	stz page

	jsr screen_clear

	pea @header
	pla
	sta str_ptr
	pla
	sta str_ptr+1
	jsr print_str
	jsr print_cr


	lda #0
	sta >IDM_AR
	sta >IDM_AR+1

	ldx #0
@loop
	lda @table,x
	sta ptr
	lda @table+1,x
	sta ptr+1
	ora ptr
	beq @done
	phx
	jsr print_register
	plx
	inx
	inx
	bra @loop

@done
	rts
;	jmp print_footer

@table
	dw @r0,@r1,@r2,@r3,@r4,@r5,@r6,@r7,@r8
	dw @r9,@r10,@r11,@r12,@r13,@r14,@r15
	dw 0


;; offset, c-string, offset, bytes,
@r0	db	$00,"MR:             ",$81,0
@r1	db	$01,"Gateway:        ",$84,0
@r2	db	$05,"Subnet:         ",$84,0
@r3	db	$09,"MAC:            ",$86,0
@r4	db	$0f,"IP:             ",$84,0
@r5	db	$15,"IR:             ",$81,XTRA_IR
@r6	db	$16,"IMR:            ",$81,0
@r7	db	$17,"RTR:            ",$82,XTRA_DEC
@r8	db 	$19,"RCR             ",$81,XTRA_DEC
@r9	db	$1a,"RMSR:           ",$81,0
@r10	db	$1b,"TMSR:           ",$81,0
@r11	db	$2a,"U IP:           ",$84,0
@r12	db	$2e,"U Port:         ",$82,XTRA_DEC
@r13	db	$1c,"PPPoE Auth      ",$82,0 ; intentionally out of order
@r14	db 	$28,"PPPoE Timer:    ",$81,0
@r15	db	$29,"PPPoE Magic:    ",$81,0

@header	db "Common  Registers",0
	modend


print_socket_header

	and #$03
	ora #$30
	sta @header+7
	pea @header
	pla
	sta str_ptr
	pla
	sta str_ptr+1
	jsr print_str
	jmp print_cr
@header db "Socket 0 Registers",0


display_0
	mx %11

	lda #1
	sta page

	jsr screen_clear

	lda #4
	sta >IDM_AR
	jsr print_socket_header
	bra reg

display_1
	mx %11

	lda #2
	sta page

	jsr screen_clear

	lda #5
	sta >IDM_AR
	jsr print_socket_header
	bra reg


display_2
	mx %11

	lda #3
	sta page

	jsr screen_clear

	lda #6
	sta >IDM_AR
	jsr print_socket_header
	bra reg

display_3
	mx %11

	lda #4
	sta page


	jsr screen_clear

	lda #7
	sta >IDM_AR
	jsr print_socket_header
;	bra reg

reg


	lda #0
	sta >IDM_AR+1

	ldx #0
@loop
	lda @table,x
	sta ptr
	lda @table+1,x
	sta ptr+1
	ora ptr
	beq @done
	phx
	jsr print_register
	plx
	inx
	inx
	bra @loop

@done
	rts
;	jmp print_footer

@table
	dw @r0,@r1,@r2,@r3,@r4,@r5,@r6,@r7,@r8
	dw @r9,@r10,@r11,@r12,@r13,@r14,@r15,@r16
	dw @r17,@r18,@r19,@r20
	dw 0

;; c-string, offset, bytes, extra
@r0	db	$00,"MR:             ",$81,XTRA_SN_MR
@r1	db	$01,"CR:             ",$81,0
@r2	db	$02,"IR:             ",$81,XTRA_SN_IR
@r3	db	$03,"SR:             ",$81,XTRA_SN_SR
@r4	db	$04,"Port:           ",$82,XTRA_DEC
@r5	db	$06,"Dest MAC:       ",$86,0
@r6	db	$0c,"Dest IP:        ",$84,0
@r7	db	$10,"Dest Port:      ",$82,XTRA_DEC
@r8	db	$12,"MSS:            ",$82,XTRA_DEC
@r9	db	$14,"Proto:          ",$81,0
@r10	db	$15,"TOS:            ",$81,0
@r11	db	$16,"TTL:            ",$81,0
@r12	db	$2d,"Fragment:       ",$82,0 ; intentionally out of order
@r13	db	$1e,"RX Buf Size:    ",$81,0
@r14	db	$1f,"TX Buf Size:    ",$81,0
@r15	db	$20,"TX FSR:         ",$82,XTRA_DEC
@r16	db	$22,"TX RD:          ",$82,0
@r17	db	$24,"TX WR:          ",$82,0
@r18	db	$26,"RX RSR:         ",$82,XTRA_DEC
@r19	db	$28,"RX RD:          ",$82,0
@r20	db	$2a,"RX WR:          ",$82,0


display_tx

	lda #PAGE_TX
	sta page


	jsr screen_clear

	pea @header
	pla
	sta str_ptr
	pla
	sta str_ptr+1
	jsr print_str
	jsr print_cr

	lda #$40
	sta >IDM_AR
	lda #0
	sta >IDM_AR+1

	bra dump_common
@header db "Transmit Buffer",0



display_rx

	lda #PAGE_RX
	sta page

	jsr screen_clear

	pea @header
	pla
	sta str_ptr
	pla
	sta str_ptr+1
	jsr print_str
	jsr print_cr

	lda #$60
	sta >IDM_AR
	lda #0
	sta >IDM_AR+1

	bra dump_common
@header db "Receive Buffer",0


dump_common

	ldy #4
	sty screen_y
	stz screen_x

	ldy #8
@yloop
	phy

	lda >IDM_AR
	jsr print_hex
	lda >IDM_AR+1
	jsr print_hex
;	lda #' '
;	jsr print
	inc screen_x

	ldx #8
@xloop1
	lda >IDM_DR
	sta xbuffer,x
	phx
	jsr print_hex
	plx
	dex
	bne @xloop1

;	lda #' '
;	jsr print
	inc screen_x

	ldx #8
@xloop2
	lda >IDM_DR
	sta xbuffer+8,x
	phx
	jsr print_hex
	plx
	dex
	bne @xloop2

	jsr print_cr

	; print the hexdump under it
	lda #5
	sta screen_x
	ldx #8
@xloop3
	phx
	lda xbuffer,x
	jsr @printc
	plx
	dex
	bne @xloop3

	inc screen_x
	ldx #8
@xloop4
	phx
	lda xbuffer+8,x
	jsr @printc
	plx
	dex
	bne @xloop4
	jsr print_cr

	ply
	dey
	bne @yloop

	rts
;	jmp print_footer

@printc
	inc screen_x
	cmp #$7f
	bcs @noc
	cmp #$20
	bcc @noc
	jmp print
@noc
	lda #'.'
	jmp print




;
; address: hex bytes...
;
;
set_memory
	ldy #23*2
	sty screen_y
	jsr readline
	bcc @ok
@err
	jmp refresh
@ok


;
; expected format:
; xx: xx xx xx xx.  space is optional
; or xxxx: xx xxx


	jsr @read.addr
	bcs @errbeep

@loop
	iny
	lda line,y
	beq @end
	cmp #' '+1
	bcc @loop ; ws
	jsr is_x
	bcs @errbeep
	ldx tmp+1
	bne @store

	asl
	asl
	asl
	asl
	sta tmp
	sec
	ror tmp+1
	bra @loop


@store	ora tmp
	sta >IDM_DR
	stz tmp
	stz tmp+1
	bra @loop

@errbeep
	_beep

@end
	jmp refresh

@read.addr
	stz tmp
	stz tmp+1

	ldy #0
	jsr @read.one
	bcs @rts
	lda line,y
	cmp #':'
	bne @16
@ar1	lda tmp
	sta >IDM_AR+1
;	iny
	clc

@rts	rts

@16
	lda tmp
	sta >IDM_AR
	jsr @read.one
	bcs @rts

	lda line,y
	cmp #':'
	beq @ar1
	sec
	rts

@read.one
	lda line,y
	jsr is_x
	bcs @rts
	asl
	asl
	asl
	asl
	sta tmp
	iny
	lda line,y
	jsr is_x
	bcs @rts
	tsb tmp
	iny
	rts


;
; does not touch x/y
;
is_x
	cmp #'0'
	bcc @no
	cmp #'9'+1
	bcc @num
	cmp #'A'
	bcc @no
	cmp #'F'+1
	bcc @letter
	cmp #'a'
	bcc @no
	cmp #'f'+1
	bcs @no
@letter
	and #$0f
	clc
	adc #9
	rts

@num	and #$0f
	clc
	rts


@no	lda #-1
	sec
	rts




	section Screen
; 
screen_init
	mx %11
	sta >$e0c00c ; disable 80-column
	sta >$e0c00f ; alternate character set

	; drop through...
screen_clear
	mx %11
	stz screen_x
	stz screen_y


	long m,x
	ldy #24*2
	ldx #0

@loop
	lda screen_table,y
	sta @smh+1
	lda #$a0a0
	ldx #40-2
@smh	sta >$e00000,x
	dex
	dex
	bpl @smh

	dey
	dey
	bpl @loop


	short m,x
	rts

line_clear
	mx %11
	long m,x

	ldy screen_y
	lda screen_table,y
	sta @smh+1
	lda #$a0a0
	ldx #40-2
@smh	sta >$e00000,x
	dex
	dex
	bpl @smh

	short m,x
	rts

print
	mx %11


	ldx screen_x
	cpx #40
	bcc @ok
	rts
@ok
	ora #$80
	pha ; save
	ldy screen_y
	lda screen_table,y
	sta @smh+1
	lda screen_table+1,y
	sta @smh+2

	pla
@smh	sta >$e00000,x
	inc screen_x
	rts

print_cr
	mx %11
	stz screen_x
	inc screen_y
	inc screen_y
	rts

print_str
	mx %11
	ldy #0
@loop
	lda (str_ptr),y
	beq @rts
	phy
	jsr print
	ply
	iny
	bra @loop
@rts	rts

print_dec
	; dibble-dabbble

	long m,x
	and #$ff
	xba
	sta <tmp
	ldx #8
	stz <bcd
	stz <bcd+2
	sed
@loop
	asl <tmp
	lda <bcd
	adc <bcd
	sta <bcd
	dex
	bne @loop
	cld

	short m,x
	jmp print_dec_helper


print_dec_16
	mx %11
	; input x/y = 16-bit number
	stx tmp
	sty tmp+1
	long m,x

	stz bcd
	stz bcd+2
	ldx #16
	sed
@loop
	asl <tmp
	lda <bcd
	adc <bcd
	sta <bcd
	lda <bcd+2
	adc <bcd+2
	sta <bcd+2
	dex
	bne @loop
	cld
	short m,x
	; drop through...

print_dec_helper
	; prints bcd number in <bcd
	clv
@5
	lda bcd+2
	beq @4
	ora #'0'
	jsr print
	bit @rts ; sev
@4
	lda bcd+1
	lsr
	lsr
	lsr
	lsr
	bvs @4a
	beq @3
@4a	ora #'0'
	jsr print
	bit @rts ; sev

@3
	lda bcd+1
	and #$0f
	bvs @3a
	beq @2
@3a	ora #'0'
	jsr print
	bit @rts ; sev

@2
	lda bcd
	lsr
	lsr
	lsr
	lsr
	bvs @2a
	beq @1
@2a	ora #'0'
	jsr print
	bit @rts ; sev

@1
	lda bcd
	and #$0f
	ora #'0'
	jsr print
@rts	rts


print_hex
	; b register must be 0.
	mx %10
	pha ; save
	lsr
	lsr
	lsr
	lsr
	tax
	lda @hex,x
	jsr print
	pla
	and #$0f
	tax
	lda @hex,x
	jmp print

@hex	db '0123456789abcdef'

screen_table

	dw $400,$480,$500,$580,$600,$680,$700,$780
	dw $428,$4a8,$528,$5a8,$628,$6a8,$728,$7a8
	dw $450,$4d0,$550,$5d0,$650,$6d0,$750,$7d0


readline	module
	mx %11

	stz line_length
	stz line

	stz screen_x
	; need to erase line...
	jsr line_clear

	lda #'_'
	jsr print
	dec screen_x

@read	lda >$e0c000
	bpl @read
	sta >$e0c010
	and #$7f

	cmp #$20
	blt @ctrl

	cmp #$7f
	beq @bs


	ldy line_length
	cpy #38
	bcs @overflow
	sta line,y
	inc line_length
	jsr print
	lda #'_'
	jsr print
	dec screen_x
	bra @read
@overflow
	; beep?
	_beep
	bra @read

@ctrl
	cmp #$08 ; backspace
	beq @bs
	cmp #$1b ; escape
	beq @esc
	cmp #$17
	beq @w
	cmp #$0d ; cr
	bne @read
@cr
	ldy line_length
	lda #0
	sta line,y
	tya
	clc
	rts

@bs
	ldy line_length
	beq @read ; beep?
	dec line_length
	dec screen_x
	lda #'_'
	jsr print
	lda #' '
	jsr print
	dec screen_x
	dec screen_x
	bra @read

@esc
	sec
	rts

; control-w - delete word.
@w
	bra @read

	modend


	section Data

saved_or	ds 2
saved_ar	ds 2

screen_x	ds 2
screen_y	ds 2

line		ds 40
line_length	ds 2

xbuffer		ds 32

