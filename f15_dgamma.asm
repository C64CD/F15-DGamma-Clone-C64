;
; F15 D'GAMMA CLONE - C64 2020 REMIX
;

; Code and graphics by TMR
; Music by aNdy


; A conversion of C64CD's Apple II intro to the C64 - coded for
; C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "f15_dgamma.prg",cbm


; Pull in the binary data
		* = $0800
		!binary "binary\f15.chr"

		* = $1000
music		!binary "binary\3d_galax.sid",,$7e

		* = $2000
		!binary "binary\plain_font_8x8.chr"

		* = $2200
screen_data	!binary "binary\f15.scr"


; Raster split position
raster_1_pos	= $00

; Label assignments
cos_at_1	= $51
cos_temp	= $52

logo_x		= $53
logo_timer	= $54
logo_count	= $55
logo_dir	= $56

rnd_1		= $57
rnd_2		= $58

scroll_count	= $59
scroll_timer	= $5a
scroll_pos	= $5b		; two bytes used

text_col_count	= $5d
text_rnd_count	= $5e

; Work spaces for the text effect
text_buffer	= $2400
effect_buffer	= $2500


; Entry point for the code
		* = $2600

; Stop interrupts
code_start	sei

; Clear the zero page
		ldx #$50
		lda #$00
zp_nuke		sta $00,x
		inx
		bne zp_nuke

; Copy the screen data down to $0400
		ldx #$00
screen_copy	lda screen_data+$000,x
		sta $0400,x
		lda screen_data+$100,x
		sta $0500,x
		lda screen_data+$200,x
		sta $0600,x
		lda screen_data+$2e8,x
		sta $06e8,x
		inx
		bne screen_copy

; Set the colour RAM to $0d
		ldx #$00
		lda #$0d
set_colour	sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dae8,x
		inx
		bne set_colour

; Set up the logo's colours
		ldx #$00
logo_colour	lda logo_colours_1,x
		sta $d850,x
		sta $d878,x
		sta $d8a0,x

		lda logo_colours_2,x
		sta $d8c8,x
		sta $d8f0,x
		sta $d918,x
		inx
		cpx #$28
		bne logo_colour

; Colour in the F15
		ldx #$00
f15_colour	ldy $0590,x
		lda f15_colours,y
		sta $d990,x

		ldy $066c,x
		lda f15_colours,y
		sta $da6c,x

		inx
		cpx #$dc
		bne f15_colour

; Initial colour set-up for the text area
		ldx #$00
		lda #$0e
text_colour	sta $db48,x
		inx
		cpx #$a0
		bne text_colour

; Clear the work spaces
		ldx #$00
work_clear	lda #$20
		sta text_buffer,x
		lda #$00
		sta effect_buffer,x
		inx
		bne work_clear

; Reset the text reader
		jsr scroll_reset
		lda #$00
		sta scroll_count
		sta scroll_timer

; Disable the ROMS and set up NMI and IRQ interrupt pointers
		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Set up the music driver
		lda #$00
		jsr music+$00

; Restart the interrupts
		cli


; Check to see if space has been pressed
main_loop	lda $dc01
		cmp #$ef
		beq *+$05
		jmp main_loop

; Reset some registers
		sei
		lda #$37
		sta $01

		lda #$00
		sta $d011
		sta $d020
		sta $d021
		sta $d418

; Reset the C64 (a linker would go here...)
		jmp $fce2


; IRQ interrupt handler
irq_int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne irq_rout
		jmp irq_exit

; An interrupt has triggered
irq_rout

; Set all of the colours for the logo
		lda #$0b
		sta $d020
		lda #$00
		sta $d021

		lda #$0b
		sta $d022
		lda #$01
		sta $d023

; Standard char mode, enable multicolour and select character set
		lda #$1b
		sta $d011
		lda #$18
		sta $d016
		lda #$12
		sta $d018

; Update the text area
		ldx #$00
text_update	lda text_buffer+$00,x
		ora effect_buffer+$00,x
		sta $0748,x
		lda text_buffer+$28,x
		ora effect_buffer+$28,x
		sta $0770,x
		lda text_buffer+$50,x
		ora effect_buffer+$50,x
		sta $0798,x
		lda text_buffer+$78,x
		ora effect_buffer+$78,x
		sta $07c0,x
		inx
		cpx #$28
		bne text_update

; Fetch a character of text
		ldx scroll_timer
		inx
		cpx #$02
		bne st_xb

		ldx scroll_count
		cpx #$a0
		bcs scroll_skip
		ldy #$00
scroll_mread	lda (scroll_pos),y
		cmp #$ff
		bne scroll_okay
		jsr scroll_reset
		jmp scroll_mread

scroll_okay	sta text_buffer,x
		inc scroll_pos+$00
		bne *+$04
		inc scroll_pos+$01

scroll_skip	inx
		stx scroll_count

		ldx #$00
st_xb		stx scroll_timer

; Randomly toggle some of the character inversions
		jsr scroll_random
		jsr scroll_random

; Fetch the current text colour...
		ldx text_col_count
		lda text_colours,x

; ...and get a "random" position to write to
		ldx text_rnd_count
		ldy text_randoms,x
		sta $db48,y

; Increment the counter for those "random" positions
		inx
		cpx #$a0
		bne col_rnd_xb

; Select a new colour if all 160 chars of the screen are done
		ldx text_col_count
		inx
		cpx #$03
		bne *+$04
		ldx #$00
		stx text_col_count

; Store the updated counter
		ldx #$00
col_rnd_xb	stx text_rnd_count

; Wait for the top of the logo's colour splits
		lda #$39
		cmp $d012
		bne *-$03

		ldx #$16
		dex
		bne *-$01

; Split the first character multicolour for the logo
		ldx #$00
logo_splitter	lda logo_rasters,x
		sta $d022	;$d022
		nop
		nop

		ldy timers,x
		dey
		bne *-$01

		inx
		cpx #$41
		bne logo_splitter

; Wait for the second colour split
		lda #$82
		cmp $d012
		bne *-$03

		ldx #$09
		dex
		bne *-$01
		nop

; Split the background colour for the F15
		ldx #$00
f15_splitter	lda f15_rasters,x
		sta $d021
		sty $d022

		ldy timers,x
		dey
		bne *-$01

		inx
		cpx #$57
		bne f15_splitter

		ldx #$07
		dex
		bne *-$01

; Set the EBCM background colours
		lda #$0b
		ldx #$02
		ldy #$09
		sta $d022
		stx $d023
		sty $d024

		lda #$08
		sta $d016

; Switch to the ROM character set for the text area, turn
; off multicolour mode and enable EBCM mode
		lda #$06
		ldx #$18
		ldy #$5b
		sta $d021
		stx $d018
		sty $d011

; Update the upper colour bars
		lda cos_at_1
		clc
		adc #$fe
		sta cos_at_1
		tay

		ldx #$00
bar_render_1	sty cos_temp

		lda bar_cosinus,y
		tay
		lda bar_data,y
		sta $0400,x
		lda bar_colour_data,y
		sta $d800,x

		lda cos_temp
		sec
		sbc #$06
		tay

		inx
		cpx #$28
		bne bar_render_1

; Update the lower colour bars
		ldx #$00
		ldy #$27
bar_render_2	lda $0400,x
		sta $0568,y
		lda $d800,x
		sta $d968,y

		dey
		inx
		cpx #$28
		bne bar_render_2

; Move one of the letters in the rolling logo
		ldy logo_x
		cpy #logo_pause
		bcs logo_move_skip

; Check which direction and call the appropriate routine
		lda logo_dir
		cmp #logo_up
		bne going_down

going_up	jsr logo_up_shift
		jmp logo_move_skip

going_down	jsr logo_down_shift

; Update the rolling logo effect counters
logo_move_skip	ldx logo_timer
		inx
		cpx #$07
		bne lt_xb

; Time to fetch a new command from the script
		ldx logo_count
		inx
		stx logo_count

logo_scr_read	lda logo_script,x
		cmp #logo_wrap
		bne logo_scr_okay

		ldx #$00
		stx logo_count
		jmp logo_scr_read

logo_scr_okay	sta logo_x

		cmp #logo_pause
		bcs logo_scr_nodir

; Fetch a direction command
		ldx logo_count
		inx
		stx logo_count

		lda logo_script,x
		sta logo_dir

logo_scr_nodir	ldx #$00
lt_xb		stx logo_timer

; Play the music
		jsr music+$03

; Set the raster position for next frame
		lda #raster_1_pos
		sta $d012

; Restore registers and exit IRQ interrupt
irq_exit	pla
		tay
		pla
		tax
		pla
nmi_int		rti


; Shunt the screen and colour data up
logo_up_shift	ldx #$00
lus_loop	lda $0450,y
		sta $0428,y
		lda $d850,y
		sta $d828,y

		lda $0478,y
		sta $0450,y
		lda $d878,y
		sta $d850,y

		lda $04a0,y
		sta $0478,y
		lda $d8a0,y
		sta $d878,y

		lda $04c8,y
		sta $04a0,y
		lda $d8c8,y
		sta $d8a0,y

		lda $04f0,y
		sta $04c8,y
		lda $d8f0,y
		sta $d8c8,y

		lda $0518,y
		sta $04f0,y
		lda $d918,y
		sta $d8f0,y

		lda $0540,y
		sta $0518,y
		lda $d940,y
		sta $d918,y

		lda $0428,y
		sta $0540,y
		lda $d828,y
		sta $d940,y

		iny
		inx
		cpx #$08
		bne lus_loop

		rts

; Shunt the screen and colour data down
logo_down_shift	ldx #$00
lds_loop	lda $0518,y
		sta $0540,y
		lda $d918,y
		sta $d940,y

		lda $04f0,y
		sta $0518,y
		lda $d8f0,y
		sta $d918,y

		lda $04c8,y
		sta $04f0,y
		lda $d8c8,y
		sta $d8f0,y

		lda $04a0,y
		sta $04c8,y
		lda $d8a0,y
		sta $d8c8,y

		lda $0478,y
		sta $04a0,y
		lda $d878,y
		sta $d8a0,y

		lda $0450,y
		sta $0478,y
		lda $d850,y
		sta $d878,y

		lda $0428,y
		sta $0450,y
		lda $d828,y
		sta $d850,y

		lda $0540,y
		sta $0428,y
		lda $d940,y
		sta $d828,y

		iny
		inx
		cpx #$08
		bne lds_loop

		rts


; Subroutine to reset the scrolling message
scroll_reset	lda #<scroll_text
		sta scroll_pos+$00
		lda #>scroll_text
		sta scroll_pos+$01
		rts

; Randomly select a character and invert it
scroll_random	lda rnd_1
		clc
		adc #$01
		eor rnd_2
		lsr
		bcc *+$04
		ora #$80
		ldx rnd_2
		sta rnd_2
		stx rnd_1

		and #$7f
		lda effect_buffer+$00,x
		clc
		adc #$40
		sta effect_buffer+$00,x
		sta effect_buffer+$80,x

		rts


; Colours for the C64CD logo
logo_colours_1	!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
		!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
		!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c

logo_colours_2	!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d

; Constants to make this next table easier to read
logo_column_1	= $00
logo_column_2	= $08
logo_column_3	= $10
logo_column_4	= $18
logo_column_5	= $20

logo_pause	= $28

logo_up		= $fd
logo_down	= $fe

logo_wrap	= $ff

; Script of X offsets for the rolling logo effect
logo_script	!byte logo_column_1,logo_up
		!byte logo_pause
		!byte logo_column_2,logo_up
		!byte logo_pause
		!byte logo_column_3,logo_up
		!byte logo_pause
		!byte logo_column_4,logo_up
		!byte logo_pause
		!byte logo_column_5,logo_up
		!byte logo_pause

		!byte logo_pause
		!byte logo_pause

		!byte logo_column_5,logo_down
		!byte logo_pause
		!byte logo_column_3,logo_down
		!byte logo_pause
		!byte logo_column_1,logo_down
		!byte logo_pause
		!byte logo_column_4,logo_down
		!byte logo_pause
		!byte logo_column_2,logo_down
		!byte logo_pause

		!byte logo_column_5,logo_up
		!byte logo_column_4,logo_up
		!byte logo_column_3,logo_up
		!byte logo_column_2,logo_up
		!byte logo_column_1,logo_up
		!byte logo_column_2,logo_up
		!byte logo_column_3,logo_up
		!byte logo_column_4,logo_up
		!byte logo_column_5,logo_up
		!byte logo_column_4,logo_up
		!byte logo_column_3,logo_up
		!byte logo_column_2,logo_up
		!byte logo_column_1,logo_up

		!byte logo_pause
		!byte logo_pause

		!byte logo_pause
		!byte logo_pause

		!byte logo_column_3,logo_down
		!byte logo_pause
		!byte logo_column_2,logo_up
		!byte logo_pause
		!byte logo_column_4,logo_down
		!byte logo_pause
		!byte logo_column_1,logo_up
		!byte logo_pause
		!byte logo_column_5,logo_down
		!byte logo_pause

		!byte logo_pause
		!byte logo_pause

		!byte logo_column_5,logo_up
		!byte logo_pause
		!byte logo_column_4,logo_down
		!byte logo_pause
		!byte logo_column_3,logo_up
		!byte logo_pause
		!byte logo_column_2,logo_down
		!byte logo_pause
		!byte logo_column_1,logo_up
		!byte logo_pause

		!byte logo_pause
		!byte logo_pause

		!byte logo_column_1,logo_up
		!byte logo_pause
		!byte logo_column_2,logo_down
		!byte logo_pause
		!byte logo_column_3,logo_down
		!byte logo_pause
		!byte logo_column_4,logo_up
		!byte logo_pause
		!byte logo_column_5,logo_up
		!byte logo_pause

		!byte logo_pause
		!byte logo_pause

		!byte logo_column_5,logo_down
		!byte logo_column_4,logo_down
		!byte logo_column_3,logo_down
		!byte logo_column_2,logo_down
		!byte logo_column_1,logo_down
		!byte logo_column_2,logo_down
		!byte logo_column_3,logo_down
		!byte logo_column_4,logo_down
		!byte logo_column_5,logo_down
		!byte logo_column_4,logo_down
		!byte logo_column_3,logo_down
		!byte logo_column_2,logo_down
		!byte logo_column_1,logo_down

		!byte logo_pause
		!byte logo_pause

		!byte logo_wrap		; end of data marker

; Character data for the bars
bar_data	!byte $00,$62,$d5,$d6,$d7,$d8,$d9,$da
		!byte $db,$dc,$dd,$de,$df,$e0,$e1,$e2
		!byte $e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea
		!byte $eb,$ec,$ed,$ee,$ef,$f0,$f1,$f2
		!byte $f3,$f4,$f5

		!byte $00,$62,$d5,$d6,$d7,$d8,$d9,$da
		!byte $db,$dc,$dd,$de,$df,$e0,$e1,$e2
		!byte $e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea
		!byte $eb,$ec,$ed,$ee,$ef,$f0,$f1,$f2
		!byte $f3,$f4,$f5

		!byte $00,$62,$d5,$d6,$d7,$d8,$d9,$da
		!byte $db,$dc,$dd,$de,$df,$e0,$e1,$e2
		!byte $e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea
		!byte $eb,$ec,$ed,$ee,$ef,$f0,$f1,$f2
		!byte $f3,$f4,$f5

		!byte $00

; Character colour data for the bars
bar_colour_data	!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
		!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
		!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
		!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
		!byte $0c,$0c,$0c

		!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
		!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
		!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
		!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
		!byte $0d,$0d,$0d

		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b

		!byte $0d

; Cosine table for the bars
bar_cosinus	!byte $65,$65,$65,$65,$65,$65,$65,$65
		!byte $65,$64,$64,$64,$63,$63,$63,$62
		!byte $62,$61,$61,$60,$60,$5f,$5f,$5e
		!byte $5d,$5d,$5c,$5b,$5a,$5a,$59,$58
		!byte $57,$56,$55,$54,$54,$53,$52,$51
		!byte $50,$4f,$4e,$4d,$4c,$4a,$49,$48
		!byte $47,$46,$45,$44,$43,$41,$40,$3f
		!byte $3e,$3d,$3c,$3a,$39,$38,$37,$36

		!byte $34,$33,$32,$31,$30,$2e,$2d,$2c
		!byte $2b,$2a,$28,$27,$26,$25,$24,$23
		!byte $22,$21,$1f,$1e,$1d,$1c,$1b,$1a
		!byte $19,$18,$17,$16,$15,$14,$13,$13
		!byte $12,$11,$10,$0f,$0f,$0e,$0d,$0c
		!byte $0c,$0b,$0a,$0a,$09,$09,$08,$08
		!byte $07,$07,$06,$06,$06,$05,$05,$05
		!byte $04,$04,$04,$04,$04,$04,$04,$04

		!byte $04,$04,$04,$04,$04,$04,$04,$04
		!byte $04,$05,$05,$05,$06,$06,$06,$07
		!byte $07,$08,$08,$09,$09,$0a,$0b,$0b
		!byte $0c,$0d,$0d,$0e,$0f,$10,$10,$11
		!byte $12,$13,$14,$15,$16,$17,$17,$18
		!byte $19,$1a,$1c,$1d,$1e,$1f,$20,$21
		!byte $22,$23,$24,$25,$27,$28,$29,$2a
		!byte $2b,$2c,$2e,$2f,$30,$31,$32,$34

		!byte $35,$36,$37,$38,$3a,$3b,$3c,$3d
		!byte $3e,$40,$41,$42,$43,$44,$45,$46
		!byte $48,$49,$4a,$4b,$4c,$4d,$4e,$4f
		!byte $50,$51,$52,$53,$54,$55,$56,$57
		!byte $57,$58,$59,$5a,$5b,$5b,$5c,$5d
		!byte $5d,$5e,$5f,$5f,$60,$60,$61,$61
		!byte $62,$62,$63,$63,$63,$64,$64,$64
		!byte $65,$65,$65,$65,$65,$65,$65,$65


; F15 colour data
f15_colours	!binary "binary\f15.col"


; Colour tables for the raster splits
		* = ((*/$100)+$01)*$100
logo_rasters	!byte $06,$06,$06,$06,$06,$06,$06,$06
		!byte $06,$06,$06,$06,$0b,$06,$06,$06
		!byte $0b,$06,$06,$0b,$06,$0b,$0b,$06
		!byte $0b,$0b,$0b,$06,$0b,$0b,$0b,$0b

		!byte $09,$09,$09,$09,$09,$09,$09,$02
		!byte $09,$09,$09,$02,$09,$09,$02,$09
		!byte $02,$02,$09,$02,$02,$02,$09,$02
		!byte $02,$02,$02,$02,$02,$02,$02,$02

		!byte $0b

f15_rasters	!byte $06,$06,$06,$06,$06,$06,$06,$06
		!byte $04,$06,$06,$06,$04,$06,$06,$04
		!byte $06,$04,$04,$06,$04,$04,$04,$06
		!byte $04,$04,$04,$04,$04,$04,$04,$04
		!byte $0e,$04,$04,$04,$0e,$04,$04,$0e
		!byte $04,$0e,$0e,$04,$0e,$0e,$0e,$04
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		!byte $09,$09,$09,$09,$09,$09,$09,$09

		!byte $08,$09,$09,$09,$08,$09,$09,$08
		!byte $09,$08,$08,$09,$08,$08,$08,$09
		!byte $08,$08,$08,$08,$08,$08,$08,$08

; Timing table for the raster splits
		* = ((*/$100)+$01)*$100
timers		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08

		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08


; Colours for the changing text
text_colours	!byte $0a,$05,$0e

; A "random" table for the changing text colours
text_randoms	!byte $50,$20,$0b,$3a,$0a,$88,$7b,$6c
		!byte $37,$35,$4d,$9b,$1a,$70,$5b,$3f
		!byte $02,$7d,$72,$8e,$00,$83,$53,$93
		!byte $80,$64,$0c,$15,$8d,$9e,$36,$2f
		!byte $1c,$28,$82,$3c,$6b,$4b,$97,$0f
		!byte $91,$1e,$26,$9c,$11,$8c,$89,$13
		!byte $5a,$19,$86,$03,$09,$7e,$40,$69
		!byte $0e,$06,$8b,$57,$62,$90,$4a,$1d

		!byte $33,$05,$7f,$66,$4f,$6e,$67,$73
		!byte $7a,$29,$52,$68,$47,$94,$01,$81
		!byte $38,$08,$41,$76,$8a,$23,$6f,$75
		!byte $07,$5e,$4e,$55,$5c,$99,$54,$2b
		!byte $56,$51,$4c,$6d,$9d,$30,$1f,$2d
		!byte $1b,$32,$60,$58,$63,$44,$22,$21
		!byte $65,$98,$2c,$12,$34,$6a,$96,$43
		!byte $27,$49,$10,$77,$3b,$79,$71,$45

		!byte $59,$78,$61,$9f,$92,$18,$84,$74
		!byte $25,$8f,$95,$16,$46,$7c,$14,$17
		!byte $2a,$87,$0d,$2e,$85,$3e,$3d,$5d
		!byte $48,$24,$5f,$9a,$39,$42,$04,$31


; The "scrolling" message
scroll_text	!scr "-=- f15 d'gamma clone -=-  the c64 remix"
		!scr "                                        "
		!scr "      coding and graphics by t.m.r      "
		!scr "music by andy  (original by ben daglish)"

		!scr " based on the apple ii crack intros for "
		!scr "   f-15 strike eagle by black bag and   "
		!scr "  lantern of d'gamma from the six pack  "
		!scr " (with a few extra c64 influences too!) "

		!scr "                                        "
		!scr "      converted to the c64 for the      "
		!scr "  csdb intro creation competition 2019  "
		!scr "                                        "

		!scr "                                        "
		!scr "                                        "
		!scr "                                        "
		!scr "                                        "

		!scr "c64cd alphabetically sorted greetings to"
		!scr "                                        "
		!scr "  1001 crew   ash and dave   black bag  "
		!scr "         borderzone dezign team         "

		!scr "         copy service stuttgart         "
		!scr "       dynamic duo         excess       "
		!scr "    four horsemen  of the apocalypse    "
		!scr "happy demomaker  harlow cracking service"

		!scr "       high-tech team       ikari       "
		!scr "   jewels        kernal        laxity   "
		!scr "  mean team     paul, shandor and matt  "
		!scr "pulse productions  reset 86  rob hubbard"

		!scr "  scoop    slipstream    stoat and tim  "
		!scr "  tangent    thalamus    the commandos  "
		!scr "   the gps   the six pack   we music    "
		!scr "  xess       yak       yeti factories   "

		!scr "                                        "
		!scr "   traditional c64cd anti-greeting to   "
		!scr "                c64hater                "
		!scr "                                        "

		!scr "                                        "
		!scr "                                        "
		!scr "                                        "
		!scr "                                        "

		!scr "and we'll finish on a quick website plug"
		!scr "                                        "
		!scr "   visit  c64crapdedunk.wordpress.com   "
		!scr "       because.... well, why not?       "

		!scr "                                        "
		!scr "             t.m.r of c64cd             "
		!scr "        shutting down 2020-01-03        "
		!scr "                                        "

		!scr "                                        "
		!scr "                                        "
		!scr "                                        "
		!scr "                                        "

		!byte $ff		; end of text marker
