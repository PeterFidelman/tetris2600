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
            ; GameBoard top and bottom caps
            lda #%11111111
            sta GameBoard           ; top
            sta GameBoard+(2*21)    ; bottom
            lda #%00001111
            sta GameBoard+1         ; top
            sta GameBoard+(2*21)+1  ; bottom

            ; GameBoard middle area
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

            ; configure PortA as input for two joysticks
            lda #$00
            sta SWACNT

            ; colors
            lda #$0E
            sta COLUPF
            lda #$70
            sta COLUBK

            jsr PieceNew
            jsr PieceLoad

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
            lda #%00000010
            sta VBLANK

            ; Set timer to count down until the end of the vertical blank
            lda #43
            sta TIM64T  ; (43*64) cpu cycles / 76 cycles/line = 36.2 scanlines

            ; Free-time for calculations here :-)
            jsr JoypadPoll
            jsr PieceIn

            ; Poll the timer until the scheduled end of the vertical blank
VertBlank:
            lda INTIM
            cmp #0
            bne VertBlank
            sta WSYNC  ; burn the last (37th) line

            ; Stop vertical blank
            lda #%00000000
            sta VBLANK

; Picture (192 scanlines)
; -----------------------
            ldx #0          ; current scanline
            ldy #0          ; current index into GameBoard
Picture:    sta WSYNC
            ; we are now in the horizontal blank for line x

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
            cpx #176        ; if we're not done drawing scanlines this frame
            bne Picture     ; ... draw the next scanline

; Scanlines 176 - 192 (16 scanlines, "lines 22, 23 of the board") need not be
; drawn (because the board only comprises line 0-21).  So this part of the
; screen is free processing time for us.
NoPicture:  sta WSYNC
            inx
            cpx #192
            bne NoPicture

; End of picture
; -------------
            ; Enter vertical blank
            lda #%00000010
            sta VBLANK

; Overscan (30 scanlines)
; -----------------------
            ; Set timer to count down until the end of the overscan
            lda #35
            sta TIM64T  ; (35*64) cpu cycles / 76 cycles/line = 29.4 scanlines

            ; Free-time for calculations here :-)
            jsr PieceOut

            ; Poll the timer until the scheduled end of overscan.
Overscan:   lda INTIM
            cmp #0
            bne Overscan
            sta WSYNC  ; burn the last (30th) line

            jmp StartOfFrame

; ----------------------------------------------------------------------
; Subroutines
MoveLeft:   ; Move a single line of the piece to the left
            asl PiecePF1.b, x
            lsr PiecePF2.b, x
            bcc MLSkip
            inc PiecePF1, x     ; set low bit (bit shifted out of PF2 into PF1)
    MLSkip: rts

MoveRight:  ; Move a single line of the piece to the right
            asl PiecePF2.b, x
            lsr PiecePF1.b, x
            bcc MRSkip
            inc PiecePF2, x     ; set low bit (bit shifted out of PF1 into PF2)
    MRSkip: rts

PieceNew:   ; TODO make this random
            ldx #0
            stx PieceR
            stx PieceS
            stx PieceX
            inx
            stx PieceY
            rts

PieceLoad:  ; From PieceR, S and X;  updates PiecePF1 and 2
            lda PieceS
            asl     ; ...*2
            asl     ; ...*4
            clc
            adc PieceR
            asl     ; ...*8
            asl     ; ...*16
            tay     ; y = (PieceS*16) + (PieceR*4)
            ldx #0
    PLNext:
            lda PieceTable.w, y
            sta PiecePF1, x
            lda #0
            sta PiecePF2, x
            iny
            inx
            cpx #4
            bne PLNext

            ; Now shift the piece left or right according to PieceX
            lda PieceX
            cmp #0      ; PieceX - 0
            beq PLDone
            bmi PLLeft
    PLRight:            ; PieceX > 0
            jsr PieceRight
            clc
            adc #$ff
            bne PLRight
            jmp PLDone
    PLLeft:             ; PieceX < 0
            jsr PieceLeft
            clc
            adc #1
            bne PLLeft
    PLDone: rts

PieceIn:    ; Mask the piece into the playfield;  ORA + STA
            lda PieceY
            asl 
            tay     ; y = PieceY * 2
            ldx #0
    PINext:
            lda PiecePF1,x
            ora GameBoard,y
            sta GameBoard,y
            iny
            lda PiecePF2,x
            ora GameBoard,y
            sta GameBoard,y
            iny
            inx
            cpx #4
            bne PINext
            rts

PieceOut:   ; Mask the piece out of the playfield;  EOR #$FF + AND + STA
            lda PieceY
            asl 
            tay     ; y = PieceY * 2
            ldx #0
    PONext:
            lda PiecePF1,x
            eor #$ff
            and GameBoard,y
            sta GameBoard,y
            iny
            lda PiecePF2,x
            eor #$ff
            and GameBoard,y
            sta GameBoard,y
            iny
            inx
            cpx #4
            bne PONext
            rts

PieceCollides:  ; Check if piece would collide;  AND;  piece must be "out".
            ; Result in zeroflag;  zero == no collision;  nonzero == collision
            lda PieceY
            asl 
            tay     ; y = PieceY * 2
            ldx #0
    PCNext:
            lda PiecePF1,x
            and GameBoard,y
            bne PCDone  ; collision;  bailout, leaving the flag "nonzero"
            iny
            lda PiecePF2,x
            and GameBoard,y
            bne PCDone  ; collision;  bailout, leaving the flag "nonzero"
            iny
            inx
            cpx #4
            bne PCNext
            ; no collision, and if we got here, the flag is "zero" :-)
    PCDone:
            rts

PieceLeft:  ; updates PiecePF1, 2
            ldx #0
            jsr MoveLeft
            inx
            jsr MoveLeft
            inx
            jsr MoveLeft
            inx
            jsr MoveLeft
            rts
PieceRight:  ; updates PiecePF1, 2
            ldx #0
            jsr MoveRight
            inx
            jsr MoveRight
            inx
            jsr MoveRight
            inx
            jsr MoveRight
            rts

IsLineFilled:   ; TODO
MoveLine:       ; TODO
ClearFilledLines:   ; TODO

JoypadPoll: ; TODO
            lda #%10000000
            bit INPT4
            beq J1Fire
            bit SWCHA
            beq J1Right
            lsr
            bit SWCHA
            beq J1Left
            lsr
            bit SWCHA
            beq J1Down
            lsr
            bit SWCHA
            beq J1Up
            lda #0          ; nothing pressed
            sta LastJoy
            jmp J1Done
    J1Fire:
            lda #1          ; fire
            cmp LastJoy
            beq J1Done      ; already pressed
            sta LastJoy     ; newly pressed
            ; fire button action
            lda PieceR
            clc
            adc #1
            and #3
            sta PieceR
            jsr PieceLoad
            jsr PieceCollides
            beq J1Fok
            lda PieceR
            clc
            adc #$ff
            and #3
            sta PieceR
            jsr PieceLoad
      J1Fok:
            jmp J1Done
    J1Right:
            lda #2          ; right
            cmp LastJoy
            beq J1Done      ; already pressed
            sta LastJoy     ; newly pressed
            ; right action
            jsr PieceRight
            inc PieceX
            jsr PieceCollides
            beq J1Rok
            jsr PieceLeft
            dec PieceX
      J1Rok:
            jmp J1Done
    J1Left:
            lda #3          ; left
            cmp LastJoy
            beq J1Done      ; already pressed
            sta LastJoy     ; newly pressed
            ; left action
            jsr PieceLeft
            dec PieceX
            jsr PieceCollides
            beq J1Lok
            jsr PieceRight
            inc PieceX
      J1Lok:
            jmp J1Done
    J1Down:
            lda #4          ; down
            cmp LastJoy
            beq J1Done      ; already pressed
            sta LastJoy     ; newly pressed
            ; down action
            inc PieceY
            jsr PieceCollides
            beq J1Dok
            dec PieceY
      J1Dok:
            jmp J1Done
    J1Up:
            lda #4          ; up
            cmp LastJoy
            beq J1Done      ; already pressed
            sta LastJoy     ; newly pressed
            ; up action
            ; TODO
            ; this is a hack
            jsr PieceIn
            jsr PieceNew
            jsr PieceLoad
    J1Done: rts
; ----------------------------------------------------------------------
; ROM data

; Piece rotation tables.
; There are 7 piece shapes, and 4 piece rotations:
;  Shapes:
;    I = 0
;    O = 1
;    T = 2
;    L = 3
;    J = 4
;    S = 5
;    Z = 6
; 
;  Rotations:
;    A = 0
;    B = 1
;    C = 2
;    D = 3
;
; The memory layout of the tables will be as follows:
;
;    Shape0 Rotation0
;       ; db (top row)
;       ; db
;       ; db
;       ; db (bottom row)
;    Shape0 Rotation1
;       ; db db db db
;    Shape0 Rotation2
;       ; ...
;    Shape0 Rotation3
;       ; ...
;
;    Shape1 Rotation0
;    Shape1 Rotation1
;    Shape1 Rotation2
;    Shape1 Rotation3
;
;    ...
;
; Each row of the table is the initial value of PF1 for a line of that piece.
; With this value of PF1, the piece will come into the playfield centered(ish).
; As the player moves the piece left & right (later) this x-movement will
; shift/ping-pong the bits between PF1 and PF2 to translate the piece left &
; right.
;
; Translating the piece up and down is simply done by changing the intial line
; at which it is OR'd into the GameBoard.
;
; PieceTable will consume (7 * 4 * 4) = 112 bytes ROM.

PieceTable:
    .DB %0000
    .DB %0000
    .DB %1111
    .DB %0000

    .DB %0100
    .DB %0100
    .DB %0100
    .DB %0100

    .DB %0000
    .DB %0000
    .DB %1111
    .DB %0000

    .DB %0100
    .DB %0100
    .DB %0100
    .DB %0100

    .DB %0000
    .DB %0110
    .DB %0110
    .DB %0000

    .DB %0000
    .DB %0110
    .DB %0110
    .DB %0000

    .DB %0000
    .DB %0110
    .DB %0110
    .DB %0000

    .DB %0000
    .DB %0110
    .DB %0110
    .DB %0000

    .DB %1110
    .DB %0100
    .DB %0000
    .DB %0000

    .DB %0100
    .DB %1100
    .DB %0100
    .DB %0000

    .DB %0100
    .DB %1110
    .DB %0000
    .DB %0000

    .DB %0100
    .DB %0110
    .DB %0100
    .DB %0000

    .DB %0000
    .DB %1110
    .DB %1000
    .DB %0000

    .DB %1100
    .DB %0100
    .DB %0100
    .DB %0000

    .DB %0010
    .DB %1110
    .DB %0000
    .DB %0000

    .DB %0100
    .DB %0100
    .DB %0110
    .DB %0000

    .DB %0000
    .DB %1110
    .DB %0010
    .DB %0000

    .DB %0100
    .DB %0100
    .DB %1100
    .DB %0000

    .DB %1000
    .DB %1110
    .DB %0000
    .DB %0000

    .DB %0110
    .DB %0100
    .DB %0100
    .DB %0000

    .DB %0110
    .DB %1100
    .DB %0000
    .DB %0000

    .DB %1000
    .DB %1100
    .DB %0100
    .DB %0000

    .DB %0110
    .DB %1100
    .DB %0000
    .DB %0000

    .DB %1000
    .DB %1100
    .DB %0100
    .DB %0000

    .DB %1100
    .DB %0110
    .DB %0000
    .DB %0000

    .DB %0100
    .DB %1100
    .DB %1000
    .DB %0000

    .DB %1100
    .DB %0110
    .DB %0000
    .DB %0000

    .DB %0100
    .DB %1100
    .DB %1000
    .DB %0000

; ----------------------------------------------------------------------
; RAM
.RAMSECTION "foo" SLOT 1
    GameBoard DS 44     ; 128 - 44 == 84 bytes of RAM remaining
    PieceX DB           ;  84 -  1 == 83    ; x-position of current piece
    PieceY DB           ;  83 -  1 == 82    ; y-position...
    PieceR DB           ;  82 -  1 == 81    ; rotation...
    PieceS DB           ;  81 -  1 == 80    ; shape ID...
    PiecePF1 DS 4       ;  80 -  4 == 76    ; PF1 bits...
    PiecePF2 DS 4       ;  76 -  4 == 72    ; PF2 bits...
    LastJoy DB          ;  72 -  1 == 71    ; Last joypad direction
.ENDS

; ----------------------------------------------------------------------
; Interrupt vectors
.ORGA $1FFA
    .DW     Reset           ; NMI
    .DW     Reset           ; RESET
    .DW     Reset           ; IRQ
; ----------------------------------------------------------------------
