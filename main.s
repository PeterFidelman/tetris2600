.INCLUDE "vcs.i"
; ----------------------------------------------------------------------
; TIATracker Constants
.DEFINE TT_GLOBAL_SPEED 1
.DEFINE TT_SPEED  7
.DEFINE TT_ODD_SPEED  7
.DEFINE TT_USE_OVERLAY  1
.DEFINE TT_USE_SLIDE  0
.DEFINE TT_USE_GOTO  1
.DEFINE TT_USE_FUNKTEMPO  0
.DEFINE TT_STARTS_WITH_NOTES  1
.DEFINE TT_FREQ_MASK    %00011111
.DEFINE TT_INS_HOLD     8
.DEFINE TT_INS_PAUSE    16
.DEFINE TT_FIRST_PERC   17

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

            ; TODO hack
            sta Buffer
            sta Buffer+2
            sta Buffer+4
            sta Buffer+6
            sty Buffer+1
            sty Buffer+3
            sty Buffer+5
            sty Buffer+7

            ; configure PortA as input for two joysticks
            lda #$00
            sta SWACNT

            ; colors
            lda #$0E
            sta COLUPF
            lda #$70
            sta COLUBK

            lda #0
            sta Level

            jsr PieceNew
            jsr PieceLoad

            ; Initialize TIATracker
            lda #0
            sta tt_cur_pat_index_c0
            lda #13
            sta tt_cur_pat_index_c1

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
            ; -----------------------------------

            ; Field the user input
            jsr JoypadPoll

            ; Decide whether it is time to tick gravity
            lda #%00011111
            ldx Level
  SOFShift: beq SOFShifted
            lsr
            dex
            jmp SOFShift
  SOFShifted:
            and FrameNo
            bne NoGravYet
            jsr GravTick    ; Tick gravity
  NoGravYet:

            ; Draw the piece
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
            lda GameBoard+1,y
            sta PF2

            ; asymmetrical playfield
            .REPEAT 12  ; hack - advance to circa color-clock 150
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
            ; -----------------------------------
            jsr PieceOut

            ; ----------------------------------------------------------------------
            ; Player

            tt_PlayerStart:

            tt_Player:
                    dec tt_timer
                    bpl xxnoNewNote
                    
                    ldx #1                          ; 2 channels
            xxadvanceLoop:
                    jsr tt_FetchNote
                    cmp #TT_INS_PAUSE
                    bcc xxfinishedNewNote	
                    bne xxnewNote
              
            xxpause:
                    lda tt_cur_ins_c0,x
                    jsr tt_CalcInsIndex
                    lda tt_InsReleaseIndexes-1,y    ; -1 b/c instruments start at #1
                    clc
                    adc #1
                    bcc xxstoreADIndex               ; unconditional

            tt_FetchNote:

            xxconstructPatPtr:
                    ldy tt_cur_pat_index_c0,x       ; get current pattern (index into tt_SequenceTable)
                    lda tt_SequenceTable,y
                    bpl xxnoPatternGoto
                    and #%01111111                  ; mask out goto bit to get pattern number
                    sta tt_cur_pat_index_c0,x       ; store goto'ed pattern index
                    bpl xxconstructPatPtr            ; unconditional
            xxnoPatternGoto:
                    tay
                    lda tt_PatternPtrLo,y
                    sta tt_ptr
                    lda tt_PatternPtrHi,y
                    sta tt_ptr+1
                    clv
                    lda tt_cur_note_index_c0,x
                    bpl xxnotPrefetched
                    and #%01111111
                    sta tt_cur_note_index_c0,x
                    bit tt_Bit6Set.w    ; PSF TEST HACK TODO - I dunno if .w is right
            xxnotPrefetched:
                    tay
                    lda (tt_ptr),y
                    bne xxnoEndOfPattern
                    sta tt_cur_note_index_c0,x      ; a is 0
                    inc tt_cur_pat_index_c0,x
                    bne xxconstructPatPtr            ; unconditional
            xxnoEndOfPattern:
                    rts

            xxnewNote:
                    sta tt_cur_ins_c0,x             ; set new instrument
                    cmp #TT_FREQ_MASK+1
                    bcs xxstartInstrument

                    tay
                    lda tt_PercIndexes-TT_FIRST_PERC,y
                    bne xxstoreADIndex               ; unconditional, since index values are >0

            xxstartInstrument:
                    bvs xxfinishedNewNote
                    jsr tt_CalcInsIndex
                    lda tt_InsADIndexes-1,y         ; -1 because instruments start at #1
            xxstoreADIndex:
                    sta tt_envelope_index_c0,x      

            xxfinishedNewNote:
                    inc tt_cur_note_index_c0,x
            xxsequencerNextChannel:
                    dex
                    bpl xxadvanceLoop

                    ldx #TT_SPEED-1
                    stx tt_timer

            xxnoNewNote:

                    ldx #1                          ; 2 channels
            xxupdateLoop:
                    lda tt_cur_ins_c0,x
                    cmp #TT_FREQ_MASK+1
                    bcs xxinstrument                 ; Melodic instrument

                    ldy tt_envelope_index_c0,x
                    lda tt_PercCtrlVolTable-1,y     ; -1 because values are stored +1
                    beq xxendOfPercussion            ; 0 means end of percussion data
                    inc tt_envelope_index_c0,x      ; if end not reached: advance index
            xxendOfPercussion:
                    sta AUDV0,x
                    lsr
                    lsr
                    lsr
                    lsr
                    sta AUDC0,x     
                    lda tt_PercFreqTable-1,y        ; -1 because values are stored +1
                    sta AUDF0,x
                    bpl xxafterAudioUpdate
                    jsr tt_FetchNote
                    cmp #TT_FREQ_MASK+1
                    bcc xxafterAudioUpdate
                    sta tt_cur_ins_c0,x             ; set new instrument
                    jsr tt_CalcInsIndex
                    lda tt_InsSustainIndexes-1,y    ; -1 because instruments start at #1
                    sta tt_envelope_index_c0,x      
                    asl tt_cur_note_index_c0,x
                    sec
                    ror tt_cur_note_index_c0,x
                    bmi xxafterAudioUpdate           ; unconditional
                
            tt_CalcInsIndex:
                    lsr
                    lsr
                    lsr
                    lsr
                    lsr
                    tay
            tt_Bit6Set:     ; This opcode has bit #6 set, for use with bit instruction
                    rts

            xxinstrument:
                    jsr tt_CalcInsIndex
                    lda tt_InsCtrlTable-1,y ; -1 because instruments start with #1
                    sta AUDC0,x
                    lda tt_envelope_index_c0,x
                    cmp tt_InsReleaseIndexes-1,y    ; -1 because instruments start with #1
                    bne xxnoEndOfSustain
                    lda tt_InsSustainIndexes-1,y    ; -1 because instruments start with #1
            xxnoEndOfSustain:
                    tay
                    lda tt_InsFreqVolTable,y
                    beq xxendOfEnvelope              ; 0 means end of release has been reached:
                    iny                             ; advance index otherwise
            xxendOfEnvelope:
                    sty tt_envelope_index_c0,x
                    sta AUDV0,x
                    lsr
                    lsr
                    lsr
                    lsr     
                    clc
                    adc tt_cur_ins_c0,x
                    sec
                    sbc #8
                    sta AUDF0,x

            xxafterAudioUpdate:
                    dex
                    bpl xxupdateLoop

            ; End Player
            ; ----------------------------------------------------------------------

            ; Poll the timer until the scheduled end of overscan.
Overscan:   lda INTIM
            cmp #0
            bne Overscan
            sta WSYNC  ; burn the last (30th) line

            inc FrameNo
            jmp StartOfFrame

; ----------------------------------------------------------------------
; Subroutines
MoveLeft:   ; Move a single line of the piece to the left
            asl PiecePF1.b, x
            lsr PiecePF2.b, x
            bcc MLSkip
            inc PiecePF1, x     ; set low bit (bit shifted out of PF2 into PF1)
            clc
    MLSkip: rts

MoveRight:  ; Move a single line of the piece to the right
            asl PiecePF2.b, x
            lsr PiecePF1.b, x
            bcc MRSkip
            inc PiecePF2, x     ; set low bit (bit shifted out of PF1 into PF2)
            clc
    MRSkip: rts

PieceNew:
            ldx #0
            stx PieceR
            stx PieceX
            ;stx PieceS ; would make piece always zero
            inx
            stx PieceY
            ; the worst "random" ever - just take some bytes from the frame
            ; counter, and if the result is out of range, pick an O-piece.
            lda FrameNo
            lsr
            lsr
            lsr
            lsr
            lsr
            clc
            and #7
            cmp #7
            bne PNDone
            lda #1
PNDone:
            sta PieceS
            rts

PieceLoad:  ; From PieceR, S and X;  updates PiecePF1 and 2
            lda PieceS
            asl     ; ...*2
            asl     ; ...*4
            clc
            adc PieceR
            asl     ; ...*8
            asl     ; ...*16
            clc
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
            clc
            bne PLRight
            jmp PLDone
    PLLeft:             ; PieceX < 0
            jsr PieceLeft
            clc
            adc #1
            clc
            bne PLLeft
    PLDone: rts

PieceIn:    ; Mask the piece into the playfield;  ORA + STA
            lda PieceY
            asl 
            clc
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
            clc
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
            clc
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

PieceLock:  ; Locks the piece and checks for horizontal lines.
            jsr PieceIn
            jsr ClearFilledLines
            jsr PieceNew
            jsr PieceLoad

LineFilled: ; Takes x (First byte of line to check).
            ; zero (beq) == full;  nonzero (bne) == not full
            lda GameBoard, x
            cmp #$ff
            bne ILFDone
            lda GameBoard+1, x
            cmp #$0f
  ILFDone:  rts

LineSlide:  ; Takes x (First byte of line to slide into (dest))
            ; and the zeropage variable SlideAmt
            txa
            sec
            sbc SlideAmt
            sec
            sbc SlideAmt
            ; hack
            cmp #40
            bmi LSOk
            lsr  ; to miss the top row
            ; note this is a 1am hack and I do not understand
            ; exactly why it worked!  D:
    LSOk:
            tay
            lda GameBoard, y
            sta GameBoard, x
            lda GameBoard+1, y
            sta GameBoard+1, x
    LSDone: rts

ClearFilledLines:
            ldx #40
            lda #0
            sta SlideAmt
   CFLTop:
            lda #0
            cmp SlideAmt
            beq CFLNoSl
            jsr LineSlide
   CFLNoSl:
            jsr LineFilled
            bne CFLNext
            ; Filled
            inc SlideAmt
            jmp CFLLoop
   CFLNext:
            dex
            dex
   CFLLoop: cpx #0
            bne CFLTop
            rts
;---
;  CFLTop:   jsr LineFilled
;            bne CFLNext
;            inc SlideAmt
;            jsr LineSlide
;  CFLNext:
;            dex
;            dex
;            cpx #0
;            bne CFLTop
;            rts
;---

;           ldx #40
;           lda #0
;           sta SlideAmt
;           sta OutOfFiller ; TODO
;    Appraise:
;;            jsr LineFilled
;;           bne DoneLine
;;           ; Filled
;;           inc SlideAmt
;;           jsr LineSlide
;;           jmp Appraise
;            lda #0
;            cmp SlideAmt
;            beq SkipSlide
;            jsr LineSlide
;  SkipSlide:jsr LineFilled
;            bne SkipInc
;            inc SlideAmt
;            jmp Appraise
;  SkipInc:  ; jmp DoneLine
;
;    DoneLine:
;            ; And move on to the next line
;            dex
;            dex
;            cpx #0
;            bne Appraise
;            rts

JoypadPoll: ; TODO
            lda #%10000000
            bit INPT4
            beq J1Fire
            bit SWCHA
            beq J1Right
            lsr
            clc
            bit SWCHA
            beq J1Left
            lsr
            clc
            bit SWCHA
            beq J1Down
            ;lsr
            ;bit SWCHA
            ;beq J1Up
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
            beq J1Rodds
            sta LastJoy     ; newly pressed
            jmp J1Rtry
      J1Rodds:
            lda FrameNo
            and #$7
            bne J1Done
      J1Rtry:
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
            beq J1Lodds
            sta LastJoy     ; newly pressed
            jmp J1LTry
      J1Lodds:
            lda FrameNo
            and #$7
            bne J1Done
      J1LTry:
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
            ;cmp LastJoy
            ;beq J1Done      ; already pressed
            sta LastJoy     ; newly pressed
            ; down action
            inc PieceY
            jsr PieceCollides
            beq J1Dok
            dec PieceY
      J1Dok:
            jmp J1Done
    J1Up:
            ;lda #5          ; up
            ;cmp LastJoy
            ;beq J1Done      ; already pressed
            ;sta LastJoy     ; newly pressed
            ;; up action
            ;jsr PieceLock
    J1Done: rts

GravTick:
            ; Can it drop?
            inc PieceY
            jsr PieceCollides
            beq GTDone  ; Yes it can, drop it.
            dec PieceY  ; No, it can't drop, put it back.
            ; And "lock piece" (meaning, move on to the next piece)
            jsr PieceLock
GTDone:     rts

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
; TIATracker Track data

tt_TrackDataStart:

tt_InsCtrlTable:
        .DB $06, $04, $04

tt_InsADIndexes:
        .DB $00, $04, $08

tt_InsSustainIndexes:
        .DB $00, $04, $08

tt_InsReleaseIndexes:
        .DB $01, $05, $09

tt_InsFreqVolTable:
        .DB $8d, $00, $8d, $00
        .DB $8f, $00, $8f, $00
        .DB $86, $00, $86, $00

tt_PercIndexes:
        .DB $01, $09

tt_PercFreqTable:
        .DB $01, $02, $03, $04, $05, $06, $87, $00
        .DB $06, $1a, $1d, $07, $08, $08, $88, $00

tt_PercCtrlVolTable:
        .DB $ed, $ed, $ed, $ed, $ed, $ec, $eb, $00
        .DB $8c, $cc, $cc, $8c, $8a, $88, $86, $00

tt_pattern0:
        .DB $11, $34, $08, $11, $12, $34, $11, $08
        .DB $08, $08, $08, $08, $12, $34, $11, $12
        .DB $11, $2f, $08, $11, $12, $08, $11, $2f
        .DB $08, $08, $08, $08, $12, $08, $08, $08
        .DB $00

tt_pattern1:
        .DB $11, $30, $08, $11, $12, $30, $11, $08
        .DB $11, $34, $08, $11, $12, $30, $11, $12
        .DB $11, $2f, $08, $11, $12, $08, $11, $2f
        .DB $08, $08, $08, $08, $2d, $08, $2c, $08
        .DB $00

tt_pattern2:
        .DB $11, $2b, $08, $11, $12, $2b, $11, $08
        .DB $08, $08, $08, $08, $12, $2b, $08, $12
        .DB $11, $2c, $08, $08, $12, $08, $11, $2c
        .DB $08, $08, $08, $08, $12, $08, $08, $08
        .DB $00

tt_pattern3:
        .DB $11, $2f, $08, $11, $12, $2f, $11, $08
        .DB $08, $08, $08, $08, $12, $2f, $11, $12
        .DB $11, $30, $08, $11, $12, $08, $11, $30
        .DB $08, $08, $08, $08, $12, $08, $08, $08
        .DB $00

tt_pattern4:
        .DB $53, $08, $10, $73, $5a, $08, $59, $08
        .DB $56, $10, $08, $76, $59, $08, $5a, $08
        .DB $5e, $10, $08, $7e, $5e, $08, $59, $08
        .DB $53, $10, $08, $73, $56, $08, $59, $08
        .DB $00

tt_pattern5:
        .DB $5a, $08, $10, $7a, $5a, $08, $59, $08
        .DB $56, $08, $10, $76, $53, $08, $10, $73
        .DB $59, $08, $10, $79, $5e, $08, $10, $7e
        .DB $5e, $08, $10, $7e, $12, $08, $08, $08
        .DB $00

tt_pattern6:
        .DB $08, $08, $08, $08, $56, $08, $52, $08
        .DB $4e, $10, $08, $6e, $50, $08, $52, $08
        .DB $53, $10, $08, $73, $10, $08, $59, $08
        .DB $53, $10, $08, $73, $56, $08, $59, $10
        .DB $00

tt_pattern7:
        .DB $5a, $08, $10, $7a, $5a, $08, $59, $08
        .DB $56, $08, $10, $76, $53, $08, $10, $73
        .DB $59, $08, $10, $79, $5e, $08, $10, $7e
        .DB $5e, $08, $10, $7e, $10, $08, $08, $08
        .DB $00

tt_pattern8:
        .DB $53, $08, $10, $08, $08, $08, $73, $10
        .DB $59, $08, $10, $08, $08, $08, $79, $10
        .DB $56, $08, $10, $08, $08, $08, $76, $10
        .DB $5a, $08, $10, $08, $08, $08, $7a, $10
        .DB $00

tt_pattern9:
        .DB $59, $08, $10, $08, $08, $08, $79, $10
        .DB $5e, $08, $10, $08, $08, $08, $7e, $10
        .DB $5f, $08, $10, $08, $08, $08, $7f, $10
        .DB $5a, $08, $10, $08, $08, $08, $7a, $10
        .DB $00

tt_pattern10:
        .DB $59, $08, $10, $79, $53, $08, $10, $73
        .DB $4e, $08, $10, $6e, $4d, $08, $10, $6d
        .DB $4f, $08, $10, $6f, $10, $08, $52, $10
        .DB $53, $10, $08, $08, $08, $08, $08, $08
        .DB $00

tt_PatternPtrLo:
        .DB <tt_pattern0, <tt_pattern1, <tt_pattern2, <tt_pattern3
        .DB <tt_pattern4, <tt_pattern5, <tt_pattern6, <tt_pattern7
        .DB <tt_pattern8, <tt_pattern9, <tt_pattern10
tt_PatternPtrHi:
        .DB >tt_pattern0, >tt_pattern1, >tt_pattern2, >tt_pattern3
        .DB >tt_pattern4, >tt_pattern5, >tt_pattern6, >tt_pattern7
        .DB >tt_pattern8, >tt_pattern9, >tt_pattern10        

tt_SequenceTable:
        ; ---------- Channel 0 ----------
        .DB $00, $01, $02, $00, $00, $01, $02, $00
        .DB $03, $03, $03, $03, $80
        
        ; ---------- Channel 1 ----------
        .DB $04, $05, $06, $07, $04, $05, $06, $07
        .DB $08, $09, $08, $0a, $8d

; ----------------------------------------------------------------------
; RAM
.RAMSECTION "foo" SLOT 1
                        ;  128 bytes RAM total
    Buffer DS 8 ; TODO  ; -  8 == 120   ; filled with |....... ....|... (lazy shift logic)
    GameBoard DS 44     ; - 44 == 76    ; the game board
    PieceX DB           ; -  1 == 75    ; x-position of current piece
    PieceY DB           ; -  1 == 74    ; y-position...
    PieceR DB           ; -  1 == 73    ; rotation...
    PieceS DB           ; -  1 == 72    ; shape ID...
    PiecePF1 DS 4       ; -  4 == 68    ; PF1 bits...
    PiecePF2 DS 4       ; -  4 == 64    ; PF2 bits...
    LastJoy DB          ; -  1 == 63    ; Last joypad direction...
    Level DB            ; -  1 == 62    ; Game level...
    FrameNo DB          ; -  1 == 61    ; Frames since start & 0xFF...
    SlideAmt DB         ; -  1 == 60    ; Num lines cleared this frame...
    ; ---
    ; TIATracker stuff
    tt_timer                ds 1    ; current music timer value
    tt_cur_pat_index_c0     ds 1    ; current pattern index into tt_SequenceTable
    tt_cur_pat_index_c1     ds 1
    tt_cur_note_index_c0    ds 1    ; note index into current pattern
    tt_cur_note_index_c1    ds 1
    tt_envelope_index_c0    ds 1    ; index into ADSR envelope
    tt_envelope_index_c1    ds 1
    tt_cur_ins_c0           ds 1    ; current instrument
    tt_cur_ins_c1           ds 1
    ; Temporary variables. These will be overwritten during a call to the
    ; player routine, but can be used between calls for other things.
    tt_ptr                  ds 2
.ENDS

; ----------------------------------------------------------------------
; Interrupt vectors
.ORGA $1FFA
    .DW     Reset           ; NMI
    .DW     Reset           ; RESET
    .DW     Reset           ; IRQ
; ----------------------------------------------------------------------
