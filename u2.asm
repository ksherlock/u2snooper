; u2 snooper CDA

; assumes slot 3.
; snasm /l /z /m/ w /g u2.asm,u2.obj,,u2.lst


	opt ws- ; relaxed white space
	opt od+ ; direct-page optimization

IDM_OR	equ $e0c0b0
IDM_AR	equ $e0c0b1
IDM_DR	equ $e0c0b3

; direct page
; rw = reserve word
; uses the __rs variable.
	rsreset
ptr	rw 1
str_ptr	rw 1 

bcd	rw 2
tmp	rw 1
scratch	rw 2 ; rl not supported...

;




pstr	macro
	db @end-@start ; n.b. - macro expansion displays the wrong value
@start
	db \1
@end
	endm

gsstr	macro
	dw @end-@start ; n.b. - macro expansion displays the wrong value
@start
	db \1
@end
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

;	jsr print_footer
	jsr keyc

mainloop

@wait	lda >$e0c000
	bpl @wait
	sta >$e0c010

	; keys
	; Q - quit
	; C - display common registers
	; 0-3 display socket registers

	and #$7f
	cmp #'a'
	blt @upper
	cmp #'z'+1
	bge @upper
	and #~$20
@upper
	cmp #'Q'
	beq quit
	ldx #@ksize-2
@loop
	cmp @k,x
	bne @next
	jsr (@d,x)
	short m,x
	bra mainloop
@next
	dex
	dex
	bpl @loop
	bra @wait 

;
; todo:
; T -> show transmit buffer
; R -> show receive buffer
; : -> write to register
; i -> clear interrupts

@k	dw '0','1','2','3','C'
@ksize	equ *-@k

@d	dw key0,key1,key2,key3,keyc




quit
	long m,x
	lda saved_or
	sta >IDM_OR
	lda saved_ar
	sta >IDM_AR

	rtl


print_footer
	mx %11
	stz screen_x
	lda #23*2
	sta screen_y

	pea @help
	pla
	sta str_ptr
	pla
	sta str_ptr+1
	jmp print_str
@help	db "Q)uit C)ommon 0 1 2 3",0

PrintOne	module
	mx %11
	ldy #0


; print the label
@loop
	lda (ptr),y
	beq load
	phy
	jsr print
	ply
	iny
	bra @loop

; read the data..
load
	iny
	lda (ptr),y
	sta >IDM_AR+1
	iny
	lda (ptr),y
	asl
	tax ; byte count / type
	phy ; save
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

extra	dw print_cr
	dw do_xtra_dec
	dw do_xtra_sr

@rts
	rts
	modend

byte1
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

do_xtra_sr	module
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
	lda (@ltable,x)
	sta str_ptr
	lda (@ltable+1,x)
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



XTRA_NOP	equ 0
XTRA_DEC	equ 2
XTRA_SR		equ 4

keyc	module
	mx %11

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
	jsr PrintOne
	plx
	inx
	inx
	bra @loop

@done
	jmp print_footer

@table
	dw @r0,@r1,@r2,@r3,@r4,@r5,@r6,@r7,@r8
	dw @r9,@r10,@r11,@r12,@r13,@r14,@r15
	dw 0


;; c-string, offset, bytes,
@r0	db	"MR:             ",0,$00,1,0
@r1	db	"Gateway:        ",0,$01,4,0
@r2	db	"Subnet:         ",0,$05,4,0
@r3	db	"MAC:            ",0,$09,6,0
@r4	db	"IP:             ",0,$0f,4,0
@r5	db	"IR:             ",0,$15,1,0
@r6	db	"IMR:            ",0,$16,1,0
@r7	db	"RTR:            ",0,$17,2,XTRA_DEC
@r8	db 	"RCR             ",0,$19,1,0
@r9	db	"RMSR:           ",0,$1a,1,0
@r10	db	"TMSR:           ",0,$1b,1,0
@r11	db	"PPPoE Auth      ",0,$1c,2,0
@r12	db 	"PPPoE Timer:    ",0,$28,1,0
@r13	db	"PPPoE Magic:    ",0,$29,1,0
@r14	db	"U IP:           ",0,$2a,4,0
@r15	db	"U Port:         ",0,$2e,2,XTRA_DEC


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


key0
	mx %11

	jsr screen_clear

	lda #4
	sta >IDM_AR
	jsr print_socket_header
	bra reg

key1
	mx %11

	jsr screen_clear

	lda #5
	sta >IDM_AR
	jsr print_socket_header
	bra reg


key2
	mx %11

	jsr screen_clear

	lda #6
	sta >IDM_AR
	jsr print_socket_header
	bra reg


key3
	mx %11

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
	jsr PrintOne
	plx
	inx
	inx
	bra @loop

@done
	jmp print_footer

@table
	dw @r0,@r1,@r2,@r3,@r4,@r5,@r6,@r7,@r8
	dw @r9,@r10,@r11,@r12,@r13,@r14,@r15,@r16
	dw @r17,@r18,@r19,@r20
	dw 0

;; c-string, offset, bytes, extra
@r0	db	"MR:             ",0,$00,1,0
@r1	db	"CR:             ",0,$01,1,0
@r2	db	"IR:             ",0,$02,1,0
@r3	db	"SR:             ",0,$03,1,0
@r4	db	"Port:           ",0,$04,2,XTRA_DEC
@r5	db	"Dest MAC:       ",0,$06,6,0
@r6	db	"Dest IP:        ",0,$0c,4,0
@r7	db	"Dest Port:      ",0,$10,2,XTRA_DEC
@r8	db	"MSS:            ",0,$12,2,XTRA_DEC
@r9	db	"Proto:          ",0,$14,1,0
@r10	db	"TOS:            ",0,$15,1,0
@r11	db	"TTL:            ",0,$16,1,0
@r12	db	"Fragment:       ",0,$2d,2,0 ; intentionally out of order
@r13	db	"RX Buf Size:    ",0,$1e,1,0
@r14	db	"TX Buf Size:    ",0,$1f,1,0
@r15	db	"TX FSR:         ",0,$20,2,0
@r16	db	"TX RD:          ",0,$22,2,0
@r17	db	"TX WR:          ",0,$24,2,0
@r18	db	"RX RSR:         ",0,$26,2,0
@r19	db	"RX RD:          ",0,$28,2,0
@r20	db	"RX WR:          ",0,$2a,2,0


dump_tx
	lda #$40
	sta >IDM_AR
	lda #0
	sta >IDM_AR+1
	bra dump_common

dump_rx
	lda #$60
	sta >IDM_AR
	lda #0
	sta >IDM_AR+1

dump_common

	ldy #18
	ldx #8
@yloop
	phy

	lda >IDM_AR
	jsr print_hex
	lda >IDM_AR+1
	jsr print_hex
	lda #' '
	jsr print

@xloop1
	lda >IDM_DR
	phx
	jsr print_hex
	dex
	bne @xloop1

	lda #' '
	jsr print

	ldx #8
@xloop2
	lda >IDM_DR
	phx
	jsr print_hex
	dex
	bne @xloop2

	jsr print_cr
	ply
	dey
	bne @yloop


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
@smh	sta >$e00000
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
;	lda <bcd+1
;	beq @2
;	clc
;	adc #'0'
;	jsr print
;	lda <bcd
;	lsr
;	lsr
;	lsr
;	lsr
;	clc
;	adc #'0'
;	jsr print
;	bra @1
;@2	lda <bcd
;	lsr
;	lsr
;	lsr
;	lsr
;	beq @1
;	clc
;	adc #'0'
;	jsr print
;@1
;	lda <bcd
;	and #$0f
;	clc
;	adc #'0'
;	jmp print

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

@read	lda >$e0c000
	bpl @read
	sta >$e0c010
	and #$7f
	cmp #$20
	blt @ctrl
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
	bra @read

@ctrl
	cmp #$08 ; backspace
	beq @bs
	cmp #$0d
	bne @read
@cr
	ldy line_length
	lda #0
	sta line_length,y
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
	dec line_length
	bra @read

	modend


	section Data

saved_or	ds 2
saved_ar	ds 2

screen_x	ds 2
screen_y	ds 2

line		ds 40
line_length	ds 2

