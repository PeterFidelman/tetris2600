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

            ; --------------------
            ; test drawing a piece
            ; --------------------

            ; initialize ram to pretend someone asked for a particular piece
            lda #$00
            sta PieceX

            lda #5
            sta PieceY

            lda #0
            sta PieceR

            lda #5
            sta PieceS

            ; load piece
            ; ----------
            ldx #0
            lda PieceS
            asl     ; ...*2
            asl     ; ...*4
            adc PieceR
            asl     ; ...*8
            asl     ; ...*16
            tay     ; y = (PieceS*16) + (PieceR*4)
    LPNext:
            lda PieceTable.w, y
            sta PiecePF1, x
            iny
            inx
            cpx #4
            bne LPNext

            ; shift piece
            ; -----------
            lda PieceX
            cmp #0      ; PieceX - 0
            beq SDone
            bmi SLeft
    SRight:             ; PieceX > 0
            ldx #0
            jsr MoveRight
            inx
            jsr MoveRight
            inx
            jsr MoveRight
            inx
            jsr MoveRight
            adc #$ff
            bne SRight
            jmp SDone
    SLeft:              ; PieceX < 0
            ldx #0
            jsr MoveLeft
            inx
            jsr MoveLeft
            inx
            jsr MoveLeft
            inx
            jsr MoveLeft
            adc #1
            bne SLeft
    SDone:

            ; commit piece
            ; ------------
            ldx #0
            lda PieceY
            asl 
            tay     ; y = PieceY * 2

    CPNext:
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
            bne CPNext

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
; With this value of PF1, the piece will come into the playfield centered.
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

; Subroutines
MoveLeft:
            asl PiecePF1.b, x
            lsr PiecePF2.b, x
            bcc MLSkip
            inc PiecePF1, x     ; set low bit (bit shifted out of PF2 into PF1)
    MLSkip: rts

MoveRight:
            asl PiecePF2.b, x
            lsr PiecePF1.b, x
            bcc MRSkip
            inc PiecePF2, x     ; set low bit (bit shifted out of PF1 into PF2)
    MRSkip: rts

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
.ENDS

; ----------------------------------------------------------------------
; Interrupt vectors
.ORGA $1FFA
    .DW     Reset           ; NMI
    .DW     Reset           ; RESET
    .DW     Reset           ; IRQ
; ----------------------------------------------------------------------
