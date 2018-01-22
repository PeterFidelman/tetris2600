; VCS.I - Hacked up WLA-DX port
; From VCS.H Version 1.05, 13/November/2003

; Here is the memory map of the Atari VCS.
; $0000 - $007F     TIA registers
; $0080 - $00FF     RAM
; $0200 - $02FF     PIA registers, only $0280-$02FF used
; $1000 - $1FFF     ROM

; ----- ROM -----
; The VCS has one SLOT (address range) through which ROM BANKs are viewed.
; $1000 - $1FFF     ROM
.MEMORYMAP
    DEFAULTSLOT 0
    SLOT 0 START $1000  SIZE $1000  ; ROM
    SLOT 1 START $0080  SIZE $0080  ; RAM
.ENDME

; The ROM BANKs.  We'll provide just one for a total of 4K ROM.
; Some cartridges have bank switching, but we're not using it here.
.ROMBANKMAP
    BANKSTOTAL 1
    BANKSIZE $1000
    BANKS 1
.ENDRO

; now we map BANK 0 into the address space at SLOT 0
.BANK 0 SLOT 0
.ORGA $1000

; ----- RAM -----
; You get to manage this yourself.  Have fun.
; $0000 - $007F     TIA registers
; $0080 - $00FF     RAM
; $0200 - $02FF     PIA registers, only $0280-$02FF used

;-------------------------------------------------------------------------------

; TIA write registers
.DEFINE VSYNC        $00 ;   0000 00x0   Vertical Sync Set-Clear
.DEFINE VBLANK       $01 ;   xx00 00x0   Vertical Blank Set-Clear
.DEFINE WSYNC        $02 ;   ---- ----   Wait for Horizontal Blank
.DEFINE RSYNC        $03 ;   ---- ----   Reset Horizontal Sync Counter
.DEFINE NUSIZ0       $04 ;   00xx 0xxx   Number-Size player/missle 0
.DEFINE NUSIZ1       $05 ;   00xx 0xxx   Number-Size player/missle 1
.DEFINE COLUP0       $06 ;   xxxx xxx0   Color-Luminance Player 0
.DEFINE COLUP1       $07 ;   xxxx xxx0   Color-Luminance Player 1
.DEFINE COLUPF       $08 ;   xxxx xxx0   Color-Luminance Playfield
.DEFINE COLUBK       $09 ;   xxxx xxx0   Color-Luminance Background
.DEFINE CTRLPF       $0A ;   00xx 0xxx   Control Playfield, Ball, Collisions
.DEFINE REFP0        $0B ;   0000 x000   Reflection Player 0
.DEFINE REFP1        $0C ;   0000 x000   Reflection Player 1
.DEFINE PF0          $0D ;   xxxx 0000   Playfield Register Byte 0
.DEFINE PF1          $0E ;   xxxx xxxx   Playfield Register Byte 1
.DEFINE PF2          $0F ;   xxxx xxxx   Playfield Register Byte 2
.DEFINE RESP0        $10 ;   ---- ----   Reset Player 0
.DEFINE RESP1        $11 ;   ---- ----   Reset Player 1
.DEFINE RESM0        $12 ;   ---- ----   Reset Missle 0
.DEFINE RESM1        $13 ;   ---- ----   Reset Missle 1
.DEFINE RESBL        $14 ;   ---- ----   Reset Ball
.DEFINE AUDC0        $15 ;   0000 xxxx   Audio Control 0
.DEFINE AUDC1        $16 ;   0000 xxxx   Audio Control 1
.DEFINE AUDF0        $17 ;   000x xxxx   Audio Frequency 0
.DEFINE AUDF1        $18 ;   000x xxxx   Audio Frequency 1
.DEFINE AUDV0        $19 ;   0000 xxxx   Audio Volume 0
.DEFINE AUDV1        $1A ;   0000 xxxx   Audio Volume 1
.DEFINE GRP0         $1B ;   xxxx xxxx   Graphics Register Player 0
.DEFINE GRP1         $1C ;   xxxx xxxx   Graphics Register Player 1
.DEFINE ENAM0        $1D ;   0000 00x0   Graphics Enable Missle 0
.DEFINE ENAM1        $1E ;   0000 00x0   Graphics Enable Missle 1
.DEFINE ENABL        $1F ;   0000 00x0   Graphics Enable Ball
.DEFINE HMP0         $20 ;   xxxx 0000   Horizontal Motion Player 0
.DEFINE HMP1         $21 ;   xxxx 0000   Horizontal Motion Player 1
.DEFINE HMM0         $22 ;   xxxx 0000   Horizontal Motion Missle 0
.DEFINE HMM1         $23 ;   xxxx 0000   Horizontal Motion Missle 1
.DEFINE HMBL         $24 ;   xxxx 0000   Horizontal Motion Ball
.DEFINE VDELP0       $25 ;   0000 000x   Vertical Delay Player 0
.DEFINE VDELP1       $26 ;   0000 000x   Vertical Delay Player 1
.DEFINE VDELBL       $27 ;   0000 000x   Vertical Delay Ball
.DEFINE RESMP0       $28 ;   0000 00x0   Reset Missle 0 to Player 0
.DEFINE RESMP1       $29 ;   0000 00x0   Reset Missle 1 to Player 1
.DEFINE HMOVE        $2A ;   ---- ----   Apply Horizontal Motion
.DEFINE HMCLR        $2B ;   ---- ----   Clear Horizontal Move Registers
.DEFINE CXCLR        $2C ;   ---- ----   Clear Collision Latches
;.ENDS
 
;-------------------------------------------------------------------------------

; TIA read registers
;                  ;                                            bit 7   bit 6
.DEFINE CXM0P        $00 ;       xx00 0000       Read Collision  M0-P1   M0-P0
.DEFINE CXM1P        $01 ;       xx00 0000                       M1-P0   M1-P1
.DEFINE CXP0FB       $02 ;       xx00 0000                       P0-PF   P0-BL
.DEFINE CXP1FB       $03 ;       xx00 0000                       P1-PF   P1-BL
.DEFINE CXM0FB       $04 ;       xx00 0000                       M0-PF   M0-BL
.DEFINE CXM1FB       $05 ;       xx00 0000                       M1-PF   M1-BL
.DEFINE CXBLPF       $06 ;       x000 0000                       BL-PF   -----
.DEFINE CXPPMM       $07 ;       xx00 0000                       P0-P1   M0-M1
.DEFINE INPT0        $08 ;       x000 0000       Read Pot Port 0
.DEFINE INPT1        $09 ;       x000 0000       Read Pot Port 1
.DEFINE INPT2        $0A ;       x000 0000       Read Pot Port 2
.DEFINE INPT3        $0B ;       x000 0000       Read Pot Port 3
.DEFINE INPT4        $0C ;       x000 0000       Read Input (Trigger) 0
.DEFINE INPT5        $0D ;       x000 0000       Read Input (Trigger) 1
;.ENDS

;-------------------------------------------------------------------------------

; PIA/RIOT registers

.DEFINE SWCHA        $280 ;      Port A data register for joysticks:
                          ;      Bits 4-7 for player 1.  Bits 0-3 for player 2.

.DEFINE SWACNT       $281 ;      Port A data direction register (DDR)
.DEFINE SWCHB        $282 ;      Port B data (console switches)
.DEFINE SWBCNT       $283 ;      Port B DDR
.DEFINE INTIM        $284 ;      Timer output
.DEFINE TIMINT       $285 ;
                          ;      $286 - $293 are unused/undefined registers
.DEFINE TIM1T        $294 ;      set 1 clock interval
.DEFINE TIM8T        $295 ;      set 8 clock interval
.DEFINE TIM64T       $296 ;      set 64 clock interval
.DEFINE T1024T       $297 ;      set 1024 clock interval
