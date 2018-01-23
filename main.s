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
            lda #$aa 
            sta ScratchPF

            lda #$1E        ; pieces are yellow
            sta COLUPF

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
            ldx #0
Picture:    sta WSYNC

            ;we are now in the overscan for line x
            ;draw stuff.
            txa
            and #$7         ; edge triggered every 8 lines
            beq doSet
            jmp doClear
    doSet:
            lda ScratchPF
            sta PF1

            ; asymmetrical playfield test
            and #$0     ; clear A
            .REPEAT 20 ; hack
                nop
            .ENDR
            sta PF1

            jmp doDone
    doClear:
            and #$0     ; clear A
            sta PF1
    doDone:
            inx
            cpx #192
            bne Picture

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
ScratchPF DB
.ENDS

; ----------------------------------------------------------------------
; Interrupt vectors
.ORGA $1FFA
    .DW     Reset           ; NMI
    .DW     Reset           ; RESET
    .DW     Reset           ; IRQ
; ----------------------------------------------------------------------
