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

            lda #$1E        ; yellow
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
Picture:    stx COLUBK
            sta WSYNC

            ;draw checkerboard background
            lda ScratchPF
            sta PF1
            eor #$ff        ; for flipped registers
            sta PF0         ; only high 4 bits used for PF0
            sta PF2
            ; writeback toggled value into ScratchPF so lines will alternate
            sta ScratchPF

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
