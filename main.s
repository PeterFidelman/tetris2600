.INCLUDE "vcs.i"
; ----------------------------------------------------------------------
; Start of ROM image
.ORGA $1000
; Clear RAM and all TIA registers
Reset:
            ldx #0
            txa
ResetLoop:  dex
            txs
            pha
            bne ResetLoop

; initialize registers and RAM

            ; game board top and bottom caps
            lda #%11111111
            sta GameBoard           ; top
            sta GameBoard+(2*21)    ; bottom
            lda #%00001111
            sta GameBoard+1         ; top
            sta GameBoard+(2*21)+1  ; bottom

            ; game board middle area
            ldx #2
            lda #%10000000
            ldy #%00001000
  InitMiddle:
            sta GameBoard,x
            inx
            sty GameBoard,x
            inx
            cpx #(2*21)
            bne InitMiddle

            ; colors
            lda #$0E
            sta COLUPF
            lda #$70
            sta COLUBK

; ----------------------------------------------------------------------
StartOfFrame:
; Vertical sync signal (3 scanlines)
; ----------------------------------
            ; Turn on VSYNC
            lda #2
            sta VSYNC

            ; Wait three scanlines
            sta WSYNC
            sta WSYNC
            sta WSYNC

            ; Turn off VSYNC
            lda #0
            sta VSYNC           

; Vertical blank (37 scanlines)
; -----------------------------
            ; Start vertical blank
            lda #%01000010
            sta VBLANK

            ldx #0
VertBlank:  sta WSYNC
            inx
            cpx #37
            bne VertBlank

            ; Stop vertical blank
            lda #%01000000
            sta VBLANK

; Picture (192 scanlines)
; -----------------------
            ldx #0          ; current scanline
            ldy #0          ; current index into GameBoard
Picture:    sta WSYNC
            ;we are now in the overscan for line x

            ; draw the game board on the left side of the playfield
            lda GameBoard,y
            sta PF1
            iny
            lda GameBoard,y
            sta PF2
            dey

            ; asymmetrical playfield
            .REPEAT 10  ; hack - advance to circa color-clock 150
                nop
            .ENDR
            and #$0     ; clear A...
            sta PF1     ; ...clear PF1 for right half of screen...
            sta PF2     ; ...and PF2.

            inx             ; prepare to move to the next scanline
            txa
            and #$7         ; every 8th line...
            beq advance     ; ... advance to the next line of the GameBoard
            jmp skip        ; ... else we're still on the the same line next time
advance:
            iny
            iny
skip:
            cpx #192        ; if we're not done drawing scanlines this frame
            bne Picture     ; ... draw the next scanline

; End of picture
; -------------
            ; Enter vertical blank
            lda #%01000010
            sta VBLANK

; Overscan (30 scanlines)
; -----------------------
            ldx #0
Overscan:   sta WSYNC
            inx
            cpx #30
            bne Overscan

            jmp StartOfFrame

; ----------------------------------------------------------------------
; RAM
.RAMSECTION "foo" SLOT 1
    GameBoard DS 44     ; 128 - 44 == 84 bytes of RAM remaining
    Row DB              ;  84 -  1 == 83
.ENDS

; ----------------------------------------------------------------------
; Interrupt vectors
.ORGA $1FFA
    .DW     Reset           ; NMI
    .DW     Reset           ; RESET
    .DW     Reset           ; IRQ
; ----------------------------------------------------------------------
