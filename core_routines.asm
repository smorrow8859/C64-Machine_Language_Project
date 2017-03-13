;===================================================================================================
;                                                                               CORE ROUTINES
;===================================================================================================
; Core routines for the framework                                             - Peter 'Sig' Hewett
;                                                                                            2016
;---------------------------------------------------------------------------------------------------
        ;-------------------------------------------------------------------------------------------
        ;                                                                             VBL WAIT
        ;-------------------------------------------------------------------------------------------
        ; Wait for the raster to reach line $f8 - if it's aleady there, wait for
        ; the next screen blank. This prevents mistimings if the code runs too fast
#region "WaitFrame"
WaitFrame
        lda VIC_RASTER_LINE         ; fetch the current raster line
        cmp #$F8                ; wait here till line #$f8
        beq WaitFrame           
        
@WaitStep2
        lda VIC_RASTER_LINE
        cmp #$F8
        bne @WaitStep2
        rts
#endregion        
        ;-------------------------------------------------------------------------------------------
        ;                                                                         UPDATE TIMERS
        ;-------------------------------------------------------------------------------------------
        ; 2 basic timers - a fast TIMER that is updated every frame,
        ; and a SLOW_TIMER updated every 16 frames
        ;-----------------------------------------------------------------------
#region "UpdateTimers"
UpdateTimers
        inc TIMER                       ; increment TIMER by 1
        lda TIMER
        and #$0F                        ; check if it's equal to 16
        beq @updateSlowTimer            ; if so we update SLOW_TIMER        
        rts

@updateSlowTimer
        inc SLOW_TIMER                  ; increment slow timer
        rts


#endregion
        ;-------------------------------------------------------------------------------------------
        ;                                                                             CLEAR SCREEN
        ;-------------------------------------------------------------------------------------------
        ; Clears the screen using a chosen character.
        ; A = Character to clear the screen with
        ; Y = Color to fill with
        ; ------------------------------------------------------------------------------------------
#region "ClearSceen"

ClearScreen
        ldx #$00                        ; Clear X register
@clearLoop
        sta SCREEN_MEM,x                ; Write the character (in A) at SCREEN_MEM + x
        sta SCREEN_MEM + 250,x          ; at SCREEN_MEM + 250 + x
        sta SCREEN_MEM + 500,x          ; at SCREEN_MEM + 500 + x
        sta SCREEN_MEM + 750,x          ; st SCREEN_MEM + 750 + x
        inx
        cpx #250                        ; is X > 250?
        bne @clearLoop                  ; if not - continue clearing

        tya                             ; transfer Y (color) to A
        ldx #$00                        ; reset x to 0
@colorLoop
        sta COLOR_MEM,x                 ; Do the same for color ram
        sta COLOR_MEM + 250,x
        sta COLOR_MEM + 500,x
        sta COLOR_MEM + 750,x
        inx
        cpx #250
        bne @colorLoop

        rts

#endregion
        ;-------------------------------------------------------------------------------------------
        ;                                                                            READ JOY 2
        ;-------------------------------------------------------------------------------------------
        ; Trying this a different way this time.  Rather than hitting the joystick registers then
        ; doing something every time - The results will be stored in JOY_X and JOY_Y with values
        ; -1 to 1 , with 0 meaning 'no input' - I should be able to just add this to a sprite for a
        ; simple move, while still being able to do an easy check for more complicated movement
        ; later on
        ;-------------------------------------------------------------------------------------------
#region "ReadJoystick"

ReadJoystick

        lda #$00                        ; Reset JOY X and Y variables
        sta JOY_X
        sta JOY_Y

        lda #$02                        ; Test for Down
        bit JOY_2
        bne @NotDown
        lda #$01
        sta JOY_Y
        jmp @NotUp                      ; Can't be up AND down
@NotDown
        lda #$01                        ; Test for Up
        bit JOY_2
        bne @NotUp
        lda #-1
        sta JOY_Y
@NotUp                                  ; Test for Left
        lda #$04
        bit JOY_2
        bne @NotLeft
        lda #-1
        sta JOY_X
        rts                             ; Can't be left AND right - no more tests

@NotLeft                                ; Test for Right
        lda #$08
        bit JOY_2
        bne @NotRight
        lda #$01
        sta JOY_X
        rts                             ; no more checks

@NotRight                               ; Nothing pressed
         rts

#endregion


        ;-------------------------------------------------------------------------------------------
        ;                                                                    JOYSTICK BUTTON PRESSED
        ;-------------------------------------------------------------------------------------------
        ; Notifies the state of the fire button on JOYSTICK 2.
        ; BUTTON_ACTION is set to one on a single press (that is when the button is released)
        ; BUTTON_PRESSED is set to 1 while the button is held down.
        ; So either a long press, or a single press can be accounted for.
        ; TODO I might put a 'press counter' in here to test how long the button is down for..
        ;-------------------------------------------------------------------------------------------
#region "JoyButton"

JoyButton

        lda #1                                  ; checks for a previous button action
        cmp BUTTON_ACTION                       ; and clears it if set
        bne @buttonTest

        lda #0                                  
        sta BUTTON_ACTION

@buttonTest
        lda #$10                                 ; test bit #4 in JOY_2 Register
        bit JOY_2
        bne @buttonNotPressed
        
        lda #1                                  ; if it's pressed - save the result
        sta BUTTON_PRESSED                      ; and return - we want a single press
        rts                                     ; so we need to wait for the release

@buttonNotPressed

        lda BUTTON_PRESSED                      ; and check to see if it was pressed first
        bne @buttonAction                       ; if it was we go and set BUTTON_ACTION
        rts

@buttonAction
        lda #0
        sta BUTTON_PRESSED
        lda #1
        sta BUTTON_ACTION

        rts

#endregion        

        ;-------------------------------------------------------------------------------------------
        ;                                                                       COPY CHARACTER SET
        ;-------------------------------------------------------------------------------------------
        ; Copy the custom character set into the VIC Memory Bank
        ; ZEROPAGE_POINTER_1 = Base address of Character Set
        ; ZEROPAGE_POINTER_2 = Target address for the Character Set
        ;
        ; Uses PARAM2
        ;-------------------------------------------------------------------------------------------
#region "CopyChars"

CopyChars
        ;lda #<CHAR_MEM                                ; set Target Address ($F000 - CHAR_MEM)
        ;sta ZEROPAGE_POINTER_2
        ;lda #>CHAR_MEM
        ;sta ZEROPAGE_POINTER_2 + 1

        loadPointer ZEROPAGE_POINTER_2, CHAR_MEM

        ldx #$00                                ; clear X, Y, A and PARAM2
        ldy #$00
        lda #$00
        sta PARAM2
@NextLine
        lda (ZEROPAGE_POINTER_1),Y              ; copy from source to target
        sta (ZEROPAGE_POINTER_2),Y

        inx                                     ; increment x / y
        iny                                     
        cpx #$08                                ; test for next character block (8 bytes)
        bne @NextLine                           ; copy next line
        cpy #$00                                ; test for edge of page (256 wraps back to 0)
        bne @PageBoundryNotReached

        inc ZEROPAGE_POINTER_1 + 1              ; if reached 256 bytes, increment high byte
        inc ZEROPAGE_POINTER_2 + 1              ; of source and target

@PageBoundryNotReached
        inc PARAM2                              ; Only copy 254 characters (to keep irq vectors intact)
        lda PARAM2
        cmp #254
        beq @CopyCharactersDone
        ldx #$00
        jmp @NextLine

@CopyCharactersDone
        rts
#endregion

        ;-------------------------------------------------------------------------------------------
        ;                                                                        COPY SPRITE DATA 
        ;-------------------------------------------------------------------------------------------
        ; Copies sprites from ZEROPAGE_POINTER_1 to ZEROPAGE_POINTER_2
        ; Sprites are copied in sets of 4
        ;-------------------------------------------------------------------------------------------
#region "CopySprites"

CopySprites
        ldy #$00
        ldx #$00

        ;lda #<SPRITE_MEM
        ;sta ZEROPAGE_POINTER_2
        ;lda #>SPRITE_MEM
        ;sta ZEROPAGE_POINTER_2 + 1

        loadPointer ZEROPAGE_POINTER_2, SPRITE_MEM


@SpriteLoop
        lda (ZEROPAGE_POINTER_1),Y
        sta (ZEROPAGE_POINTER_2),Y
        iny
        bne @SpriteLoop
        inx
        inc ZEROPAGE_POINTER_1 + 1
        inc ZEROPAGE_POINTER_2 + 1
        cpx #NUMBER_OF_SPRITES_DIV_4
        bne @SpriteLoop

        rts

#endregion
        ;-------------------------------------------------------------------------------------------
        ;                                                                       DISPLAY TEXT
        ;-------------------------------------------------------------------------------------------
        ; Displays a line of text.      '@' ($00) is the end of text character
        ;                               '/' ($2f) is the line break character
        ; ZEROPAGE_POINTER_1 = pointer to text data
        ; PARAM1 = X
        ; PARAM2 = Y
        ; PARAM3 = Color
        ; Modifies ZEROPAGE_POINTER_2 and ZEROPAGE_POINTER_3
        ;
        ; NOTE : all text should be in lower case :  byte 'hello world@' or byte 'hello world',$00
        ;-------------------------------------------------------------------------------------------
#region "DisplayText"


DisplayText

        ldx PARAM2

        lda SCREEN_LINE_OFFSET_TABLE_LO,x

        sta ZEROPAGE_POINTER_2
        sta ZEROPAGE_POINTER_3
        lda SCREEN_LINE_OFFSET_TABLE_HI,x
        sta ZEROPAGE_POINTER_2 + 1

        clc
;       adc #((COLOR_MEM - SCREEN_MEM) & 0xff00) >> 8
        adc #>COLOR_DIFF
        sta ZEROPAGE_POINTER_3 + 1

        lda ZEROPAGE_POINTER_2
        clc
        adc PARAM1
        sta ZEROPAGE_POINTER_2
        lda ZEROPAGE_POINTER_2 + 1
        adc #0
        sta ZEROPAGE_POINTER_2 + 1
        lda ZEROPAGE_POINTER_3
        clc
        adc PARAM1
        sta ZEROPAGE_POINTER_3
        lda ZEROPAGE_POINTER_3 + 1
        adc #0
        sta ZEROPAGE_POINTER_3 + 1

        ldy #0
@inlineLoop
        lda (ZEROPAGE_POINTER_1),y              ; test for end of line
        cmp #$00
        beq @endMarkerReached                 
        cmp #$2F                                ; test for line break
        beq @lineBreak
        sta (ZEROPAGE_POINTER_2),y
        lda PARAM3
        sta (ZEROPAGE_POINTER_3),y
        iny
        jmp @inLineLoop

@lineBreak
        iny
        tya
        clc
        adc ZEROPAGE_POINTER_1
        sta ZEROPAGE_POINTER_1
        lda #0
        adc ZEROPAGE_POINTER_1 + 1
        sta ZEROPAGE_POINTER_1 + 1

        inc PARAM2
        
        jmp DisplayText

@endMarkerReached
        rts

#endregion

;---------------------------------------------------------------------------------------------------
;                                                                               DISPLAY BYTE DATA
;---------------------------------------------------------------------------------------------------
; Displays the data stored in a given byte on the screen as readable text in hex format (0-F)

; X = screen line                - Yes, this is a little arse-backwards (X and Y) but I don't think
; Y = screen column                addressing modes allow me to swap them around
; A = byte to display
; MODIFIES : ZEROPAGE_POINTER_1, ZEROPAGE_POINTER_3, PARAM4
;---------------------------------------------------------------------------------------------------
; Ken Jennings -- modified this to use a lookup table for the Nybble to screen text conversion.
; This also changes the function to write in left to right order, so callers may need to 
; adjust their X coordinate.
; (Also, this change is added to give Steve a reason to work with branches and pull requests)   :-)
;---------------------------------------------------------------------------------------------------

#region "DisplayByte"
DisplayByte

        sta PARAM4                                      ; store the byte to display in PARAM4

        lda SCREEN_LINE_OFFSET_TABLE_LO,x               ; look up the address for the screen line
        sta ZEROPAGE_POINTER_1                          ; store lower byte for address for screen
        sta ZEROPAGE_POINTER_3                          ; and color
        lda SCREEN_LINE_OFFSET_TABLE_HI,x               ; store high byte for screen
        sta ZEROPAGE_POINTER_1 + 1
        clc
        adc #>COLOR_DIFF                                ; add the difference to color mem
        sta ZEROPAGE_POINTER_3 + 1                      ; for the color address high byte

        lda PARAM4                                      ; load the byte to be displayed
        
        lsr  ;; divide by 16 to shift hi nybble into low nybble ( value of 0-F)
	lsr
	lsr
	lsr
	tax 
	lda NYBBLE_TO_HEX,x  ;; simplify. no math.  just lookup table.
        
	sta (ZEROPAGE_POINTER_1),y                      ; write the character code
        lda #COLOR_WHITE                                ; set the color to white
        sta (ZEROPAGE_POINTER_3),y                      ; write the color to color ram

	iny ;; writes left to right.

        lda PARAM4                                      ; fetch the byte to DisplayText
                                      
	and #$0F ;; low nybble is second character
        tax
	lda NYBBLE_TO_HEX,x  ;; simplify. no math.  just lookup table.
        
        sta (ZEROPAGE_POINTER_1),y                      ; write character and color
        lda #COLOR_WHITE                                ; set the color to white
        sta (ZEROPAGE_POINTER_3),y                      ; write the color to color ram
       
        rts

NYBBLE_TO_HEX ;; Values in Atari format
	byte '0123456789ABCDEF'  ; C64 PETSCII
;;      .sbyte "0123456789ABCDEF" ; Atari internal format.

#endregion
;---------------------------------------------------------------------------------------------------
        

