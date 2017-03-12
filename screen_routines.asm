;===============================================================================
;                                                               SCREEN ROUTINES
;===============================================================================
;                                                            Peter 'Sig' Hewett
;                                                                       - 2016
;-------------------------------------------------------------------------------
; Routines to draw on or modify the screen
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;                                                         DRAW VERTICAL LINE
;-------------------------------------------------------------------------------
; DrawVLine - draws a vertical line with a specified color and character.
;             it's not optimized or terribly pretty but it lets you reuse your
;             PARAM variables to draw another line straight away
;
; PARAM1 = start X
; PARAM2 = start Y
; PARAM3 = end Y
; PARAM4 = character
; PARAM5 = color
;-------------------------------------------------------------------------------
#region "DrawVLine"

DrawVLine
        ldx PARAM2                              ; fetch the start address in X (Y coord)
        ldy PARAM1                              ; setup Y register for column (X coord)
@loop
        lda SCREEN_LINE_OFFSET_TABLE_LO,x       ; Fetch the address of the start line and
        sta ZEROPAGE_POINTER_1                  ; store it in ZEROPAGE_POINTER_1
        sta ZEROPAGE_POINTER_2                  ; ZEROPAGE_POINTER_2 will hold the color address
        lda SCREEN_LINE_OFFSET_TABLE_HI,x
        sta ZEROPAGE_POINTER_1 + 1

        clc
        adc #>COLOR_DIFF                        ; add the difference to color memory
        sta ZEROPAGE_POINTER_2 + 1              ; ZEROPAGE_POINTER_2 now has the correct address

        lda PARAM4                              ; load the character
        sta (ZEROPAGE_POINTER_1),y              ; write the character to the screen position
        lda PARAM5                              ; load the color
        sta (ZEROPAGE_POINTER_2),Y              ; write it to color ram

        inx
        clc                                     ; increment X
        cpx PARAM3                              ; check against the end position
        bcc @loop                               ; if not equal - loop back
        
        rts

#endregion
;-------------------------------------------------------------------------------
;                                                         DRAW HORIZONTAL LINE
;-------------------------------------------------------------------------------
; DrawVLine - draws a horizontal line with a specified color and character.
;             it's not optimized or terribly pretty but it lets you reuse your
;             PARAM variables to draw another line straight away
;
; PARAM1 = start X
; PARAM2 = start Y
; PARAM3 = end X
; PARAM4 = character
; PARAM5 = color
;-------------------------------------------------------------------------------
#region "DrawHLine"

DrawHLine
        ldx PARAM2                      ; load the Y coordinate for the lookup tables
        ldy PARAM1                      ; start X coord in Y register

@loop
        lda SCREEN_LINE_OFFSET_TABLE_LO,x       ; load the screen line address
        sta ZEROPAGE_POINTER_1
        sta ZEROPAGE_POINTER_2                  ; fetch the low byte for the color address
        lda SCREEN_LINE_OFFSET_TABLE_HI,x
        sta ZEROPAGE_POINTER_1 + 1

        clc
        adc #>COLOR_DIFF                ; add the difference to color ram
        sta ZEROPAGE_POINTER_2 + 1

        lda PARAM4
        sta (ZEROPAGE_POINTER_1),y
        lda PARAM5
        sta (ZEROPAGE_POINTER_2),y

        iny                             ; increment y (our X coordinate
        clc
        cpy PARAM3                      ; compare it to PARAM3 - end X coordinate
        bcc @loop                       ; if less than, loop back

        rts
#endregion    

#region "UpdateScroll"

;UpdateScroll
;        lda TIMER                       ;temp timer for debugging
;        and #%0011                      ;actually scroll every 3rd frame

;                                        ;assuming scrolling right
;        dec SCROLL_COUNT
;        lda SCROLL_COUNT
;        and #%00000111                  ;mask it 0 - 7
;        sta SCROLL_COUNT
;        cmp #0
;        bne @frame7
;        jsr CopyCharData

;@frame

;       cmp #7
;       bne @done
;       jsr CopyCharData
;       jsr ColorScroll
;       jsr SwapScreens
;       rts


;       SCROLL TIMING FOR ONE PIXEL SCROLL RIGHT

;       0       - Copy jump from front screen to buffer screen
;       1       - Upper half color rem shift (interrupt)
;       2       - Character shift part 2 (right)
;       3       - Character shift part 1 (right)
;       4       - Neutral position - center of character
;       5
;       6
;       7       - swap screens/update pointers/jump color

;@cont

;@done
;       rts

;#endregion

;-----------------------------------------------------------------
;                       SWAP SCREENS
;
; Swap the poiinter of the current screen
;-----------------------------------------------------------------

;SwapScreens

;       lda CURRENT_SCREEN+1
;       cmp #$44
;       beq @screen2

;       lda #<SCREEN2_MEM
;       sta CURRENT_SCREEN
;       lda #>SCREEN2_MEM
;       sta CURRENT_SCREEN + 1
;       rts

;@screen2

;;       loadPointer CURRENT_SCREEN, SCREEN1_MEM

;       lda #<SCREEN1_MEM
;       sta CURRENT_SCREEN
;       lda #>SCREEN1_MEM
;       sta CURRENT_SCREEN + 1
;       rts

;-----------------------------------------------------------------
;               COPY CHAR DATA
;-----------------------------------------------------------------

;-----------------------------------------------------------------
; Copys the current screen to the backbuffer one character over
; These routines are unrolled for speed
;-----------------------------------------------------------------
;CopyCharData

;;      jmp CopyScroll

;       lda CURRENT_SCREEN +_1          ;The high byte of the screen screen address
;       cmp #$44
;       beq @screen2

;@screen2
;       jmp CopyFmScreen2
;       rts
 
;CopyScroll      
;       ldx #0                          ;for testing we're not using the backbuffer
;@copyloop
;       lda SCREEN1_MEM + 1,x
;       sta SCREEN1_MEM,x

;       lda SCREEN1_MEM + 41,x
;       sta SCREEN1_MEM + 40,x

;       lda SCREEN1_MEM + 81,x
;       sta SCREEN1_MEM + 80,x

;       lda SCREEN1_MEM + 121,x
;       sta SCREEN1_MEM + 120,x

;       lda SCREEN1_MEM + 161,x
;       sta SCREEN1_MEM + 160,x

;       lda SCREEN1_MEM + 281,x
;       sta SCREEN1_MEM + 280,x

;       lda SCREEN1_MEM + 321,x
;       sta SCREEN1_MEM + 320,x

;       lda SCREEN1_MEM + 361,x
;       sta SCREEN1_MEM + 360,x

;       lda SCREEN1_MEM + 401,x
;       sta SCREEN1_MEM + 400,x

;       lda SCREEN1_MEM + 441,x
;       sta SCREEN1_MEM + 440,x

;       lda SCREEN1_MEM + 481,x
;       sta SCREEN1_MEM + 480,x

;       lda SCREEN1_MEM + 521,x
;       sta SCREEN1_MEM + 520,x

;       lda SCREEN1_MEM + 561,x
;       sta SCREEN1_MEM + 560,x

;       lda SCREEN1_MEM + 601,x
;       sta SCREEN1_MEM + 600,x

;       lda SCREEN1_MEM + 641,x
;       sta SCREEN1_MEM + 640,x

;       lda SCREEN1_MEM + 681,x
;       sta SCREEN1_MEM + 680,x

;       lda SCREEN1_MEM + 721,x
;       sta SCREEN1_MEM + 720,x

;       ldx
;       cpx #38
;       bne @copyloop
;       rts

;Copy from Screen 1
;CopyFmScreen1

;       ldx #0

;@copyloop

;       lda SCREEN1_MEM + 1,x
;       sta SCREEN2_MEM,x

;       lda SCREEN1_MEM + 81,x
;       sta SCREEN2_MEM + 80,x

;       lda SCREEN1_MEM + 121,x
;       sta SCREEN2_MEM + 120,x

;       lda SCREEN1_MEM + 161,x
;       sta SCREEN2_MEM + 160,x

;       lda SCREEN1_MEM + 281,x
;       sta SCREEN2_MEM + 280,x

;       lda SCREEN1_MEM + 321,x
;       sta SCREEN2_MEM + 320,x

;       lda SCREEN1_MEM + 361,x
;       sta SCREEN2_MEM + 360,x

;       lda SCREEN1_MEM + 401,x
;       sta SCREEN2_MEM + 400,x

;       lda SCREEN1_MEM + 441,x
;       sta SCREEN2_MEM + 440,x

;       lda SCREEN1_MEM + 481,x
;       sta SCREEN2_MEM + 480,x

;       lda SCREEN1_MEM + 521,x
;       sta SCREEN2_MEM + 520,x

;       lda SCREEN1_MEM + 561,x
;       sta SCREEN2_MEM + 560,x

;       lda SCREEN1_MEM + 601,x
;       sta SCREEN2_MEM + 600,x

;       lda SCREEN1_MEM + 641,x
;       sta SCREEN2_MEM + 640,x

;       lda SCREEN1_MEM + 681,x
;       sta SCREEN2_MEM + 680,x

;       lda SCREEN1_MEM + 721,x
;       sta SCREEN2_MEM + 720,x

;       inx
;       cpx #38
;       bne @copyloop
;       rts

;CopyFmScreen2

;       lda #COLOR_RED
;       sta VIC_BORDER_COLOR
;       ldx #0
;@copyloop
;       lda SCREEN2_MEM + 1,x
;       sta SCREEN1_MEM,x

;       lda SCREEN2_MEM + 41,x
;       sta SCREEN1_MEM + 40,x

;       lda SCREEN2_MEM + 81,x
;       sta SCREEN1_MEM + 80,x

;       lda SCREEN2_MEM + 121,x
;       sta SCREEN1_MEM + 120,x

;       lda SCREEN2_MEM + 161,x
;       sta SCREEN1_MEM + 160,x

;       lda SCREEN2_MEM + 281,x
;       sta SCREEN1_MEM + 280,x

;       lda SCREEN2_MEM + 321,x
;       sta SCREEN1_MEM + 320,x

;       lda SCREEN2_MEM + 361,x
;       sta SCREEN1_MEM + 360,x

;       lda SCREEN2_MEM + 401,x
;       sta SCREEN1_MEM + 400,x

;       lda SCREEN2_MEM + 441,x
;       sta SCREEN1_MEM + 440,x

;       lda SCREEN2_MEM + 481,x
;       sta SCREEN1_MEM + 480,x

;       lda SCREEN2_MEM + 521,x
;       sta SCREEN1_MEM + 520,x

;       lda SCREEN2_MEM + 561,x
;       sta SCREEN1_MEM + 560,x

;       lda SCREEN2_MEM + 601,x
;       sta SCREEN1_MEM + 600,x

;       lda SCREEN2_MEM + 641,x
;       sta SCREEN1_MEM + 640,x

;       lda SCREEN2_MEM + 681,x
;       sta SCREEN1_MEM + 680,x

;       lda SCREEN2_MEM + 721,x
;       sta SCREEN1_MEM + 720,x

;       inx
;       cpx #38
;       bne @copyloop
;       rts

;ColorScroll

;       ldx #0
;@copyloop
;       lda COLOR_MEM + 1,x
;       sta COLOR_MEM,x

;       lda COLOR_MEM + 41,x
;       sta COLOR_MEM + 40,x

;       lda COLOR_MEM + 81,x
;       sta COLOR_MEM + 80,x

;       lda COLOR_MEM + 121,x
;       sta COLOR_MEM + 120,x

;       lda COLOR_MEM + 161,x
;       sta COLOR_MEM + 160,x

;       lda COLOR_MEM + 281,x
;       sta COLOR_MEM + 280,x

;       lda COLOR_MEM + 321,x
;       sta COLOR_MEM + 320,x

;       lda COLOR_MEM + 361,x
;       sta COLOR_MEM + 360,x

;       lda COLOR_MEM + 401,x
;       sta COLOR_MEM + 400,x

;       lda COLOR_MEM + 441,x
;       sta COLOR_MEM + 440,x

;       lda COLOR_MEM + 481,x
;       sta COLOR_MEM + 480,x

;       lda COLOR_MEM + 521,x
;       sta COLOR_MEM + 520,x

;       lda COLOR_MEM + 561,x
;       sta COLOR_MEM + 560,x

;       lda COLOR_MEM + 601,x
;       sta COLOR_MEM + 600,x

;       lda COLOR_MEM + 641,x
;       sta COLOR_MEM + 640,x

;       lda COLOR_MEM + 681,x
;       sta COLOR_MEM + 680,x

;       lda COLOR_MEM + 721,x
;       sta COLOR_MEM + 720,x

;       inx
;       cpx #38
;       bne @copyloop
;       rts