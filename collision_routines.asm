;===============================================================================
;                                                           COLLISION ROUTINES
;===============================================================================
;                                                            Peter 'Sig' Hewett
;                                                                       - 2016
;-------------------------------------------------------------------------------
; Routines dealing with collisions between game objects
;-------------------------------------------------------------------------------

;===============================================================================
;                                               SPRITE TO BACKGROUND CHARACTERS
;===============================================================================
; Checks to see if the sprite is colliding with a background character.
; Many of these checks will be 'forward looking' (especially in movement checks)
; We will be looking to where the sprite will be, not where it is, and then
; letting the sprite handling routines update the positions and deltas for us
; if we move.
;-------------------------------------------------------------------------------

;--------------------------------------------------------------------------------
;                                                               CAN MOVE LEFT
;--------------------------------------------------------------------------------
; Checks ahead to see if this sprite can move left, or if it's going to to be
; stopped by a blocking character.
;
; X = sprite we want to check for
;
; returns A = 0 we can move or A = 1 we are blocked
; X register is left intact
;--------------------------------------------------------------------------------
#region "CanMoveLeft"


CanMoveLeft
                                                ; border test
        lda SPRITE_CHAR_POS_X                   ; if Char X is 0
        bne @trimLeft
        lda SPRITE_POS_X_DELTA,x                ; and delta is 0
        bne @trimLeft
        lda #1                                  ; return blocked
        rts
@trimLeft
        lda SPRITE_POS_X_DELTA,x        ; fetch the X delta for this sprite
        adc SPRITE_DELTA_TRIM_X,x       ; add delta trim X
        and #%111                       ; Mask the result for 0-7

        beq @checkLeft                  ; if delta != 0 no need to check for a blocking
                                        ; character - we're not flush with the char set
        lda #0                          ; load a return code of #0 and return
        rts

@checkLeft
        lda SPRITE_POS_Y_DELTA,x        ; if the Y Delta is 0, we only need to check 2 characters
        beq @checkLeft2                 ; on the direct left of the sprite base because we are 'flush'
                                        ; on the Y axis with the character map

                                        ; Here we aren't flush - so we have to check 3 characters

        ldy SPRITE_CHAR_POS_Y,x         ; fetch sprites Y character position (in screen memory)
        lda SCREEN_LINE_OFFSET_TABLE_LO,y       ; store the address in ZEROPAGE_POINTER_1
        sta ZEROPAGE_POINTER_1
        lda SCREEN_LINE_OFFSET_TABLE_HI,y
        sta ZEROPAGE_POINTER_1 + 1
        
        lda SPRITE_CHAR_POS_X,x                 ; fetch sprites X position (in screen memory)
        clc
        adc #39                                  ; add 39 (go down one row and one place to the left)
        tay                                     ; store it in the Y register
        lda (ZEROPAGE_POINTER_1),y              ; fetch the character from screen mem

        jsr TestBlocking                        ; test to see if it blocks
        bne @blockedLeft                        ; returned 1  - so blocked

@checkLeft2
        ldy SPRITE_CHAR_POS_Y,x                 ; fetch the sprites Y position and store it in Y
        dey                                     ; decrement by 1 (so 1 character UP)
        lda SCREEN_LINE_OFFSET_TABLE_LO,y       ; store that memory location in ZEROPAGE_POINTER_1
        sta ZEROPAGE_POINTER_1
        lda SCREEN_LINE_OFFSET_TABLE_HI,y 
        sta ZEROPAGE_POINTER_1 + 1

        ldy SPRITE_CHAR_POS_X,x                 ; get the sprites X position and store in Y
        dey                                     ; decrement by 1 (one character left)
        
        lda (ZEROPAGE_POINTER_1),y              ; fetch the contents of screen mem 1 left and 1 up
        jsr TestBlocking                        ; test for blocking
        bne @blockedLeft

        tya                                     ; transfer screen X pos to the accumulator
        clc             
        adc #40                                 ; add 40 - bringing it one row down from the last
        tay                                     ; check made, then transfer it back to Y
        
        lda (ZEROPAGE_POINTER_1),y              ; fetch the character from that screen location
        jsr TestBlocking                        ; and test it for blocking
        bne @blockedLeft


        lda #0                                  ; return value #0 = not blocked
        rts

@blockedLeft
        lda #1                       ; we can't move, so load a #1 in A and return
        rts

#endRegion

;--------------------------------------------------------------------------------
;                                                               CAN MOVE RIGHT
;--------------------------------------------------------------------------------
; Checks ahead to see if this sprite can move right, or if it's going to to be
; stopped by a blocking character.
;
; X = sprite we want to check for
;
; returns A = 0 we can move or A = 1 we are blocked
; X register is left intact
;---------------------------------------------------------------------------------
#region "CanMoveRight"

CanMoveRight
        clc                             ; simple right border check
        lda SPRITE_CHAR_POS_X,x         ; sprite is < $26
        cmp #$26
        bne @trimRight
        clc
        lda SPRITE_POS_X_DELTA,x         ; and delta < $04
        cmp #4
        bcc @trimRight
        
        lda #1                          ; return blocked
        rts
        
@trimRight
        lda SPRITE_POS_X_DELTA,x        ; if Delta = 0, perform checks
        adc SPRITE_DELTA_TRIM_X,x       ; add delta trim
        and #%111
        beq @checkRight

        lda #0                          ; we can move, return 0
        rts

@checkRight
        lda SPRITE_POS_Y_DELTA,x       ; flush check on Y - if so, only check 2 chars
        beq @rightCheck2

        ldy SPRITE_CHAR_POS_Y,x         ; Check third character position - we are not flush with
        iny                             ; the screen character coords
        lda SCREEN_LINE_OFFSET_TABLE_LO,y
        sta ZEROPAGE_POINTER_1
        lda SCREEN_LINE_OFFSET_TABLE_HI,y
        sta ZEROPAGE_POINTER_1 + 1
        
        ldy SPRITE_CHAR_POS_X,x                 ; fetch sprites X position, store it in Y
        iny
        iny

        lda (ZEROPAGE_POINTER_1),y
        jsr TestBlocking
        bne @blockedRight
        

@rightCheck2
        ldy     SPRITE_CHAR_POS_Y,x
        dey
        lda SCREEN_LINE_OFFSET_TABLE_LO,y
        sta ZEROPAGE_POINTER_1
        lda SCREEN_LINE_OFFSET_TABLE_HI,y
        sta ZEROPAGE_POINTER_1 + 1

        ldy SPRITE_CHAR_POS_X,x
        iny
        iny

        lda (ZEROPAGE_POINTER_1),y

        jsr TestBlocking
        bne @blockedRight

        tya
        clc
        adc #40
        tay
        lda (ZEROPAGE_POINTER_1),y
        jsr TestBlocking
        bne @blockedRight

        lda #0
        rts

@blockedRight
        lda #1
        rts

#endregion

;--------------------------------------------------------------------------------
;                                                               CAN MOVE UP
;--------------------------------------------------------------------------------
; Checks ahead to see if this sprite can move up, or if it's going to to be
; stopped by a blocking character.
;
; X = sprite we want to check for
;
; returns A = 0 we can move or A = 1 we are blocked
; X register is left intact
;---------------------------------------------------------------------------------
#region "CanMoveUp"
CanMoveUp
        lda SPRITE_POS_Y_DELTA,x        ; load Delta Y value
        beq @checkUp                    ; if it's 0 we need to check characters

        lda #0                          ; if not we can just return and move
        rts

@checkUp
        lda SPRITE_POS_X_DELTA,x        ; Check X delta - if 0 we only need to check one
;        adc SPRITE_DELTA_TRIM_X,x       ; add our trim and make keep within 0-7 range
;        and #%111
        
        beq @checkUp2                   ; character above the player

                                        ; else we are not flush on X and need to check 2

        ldy SPRITE_CHAR_POS_Y,x         ; fetch the sprite Y char coord - store in Y
        dey
        dey                             ; subtract 2

        lda SCREEN_LINE_OFFSET_TABLE_LO,y       ; fetch the address of screen line address
        sta ZEROPAGE_POINTER_1
        lda SCREEN_LINE_OFFSET_TABLE_HI,y
        sta ZEROPAGE_POINTER_1 + 1

        ldy SPRITE_CHAR_POS_x,x                 ; fetch X position
        iny                                     ; add one
;        iny                                     ; extra for trim

        lda (ZEROPAGE_POINTER_1),y

        jsr TestBlocking
        bne @upBlocked

@checkUp2
        ldy SPRITE_CHAR_POS_Y,x                 ; get the sprite Y char coordinate
        dey                                     ; subtract 2
        dey
        lda SCREEN_LINE_OFFSET_TABLE_LO,y       ; fetch the address of that line
        sta ZEROPAGE_POINTER_1
        lda SCREEN_LINE_OFFSET_TABLE_HI,y
        sta ZEROPAGE_POINTER_1 + 1

        ldy SPRITE_CHAR_POS_X,x                 ; fetch the sprite X char coordinate
;        iny                                     ; add one for trim

        lda (ZEROPAGE_POINTER_1),y

        jsr TestBlocking
        bne @upBlocked

        lda #0
        rts
        
@upBlocked
        lda #1
        rts

#endregion

;--------------------------------------------------------------------------------
;                                                               CAN MOVE DOWN
;--------------------------------------------------------------------------------
; Checks ahead to see if this sprite can move up, or if it's going to to be
; stopped by a blocking character.
;
; X = sprite we want to check for
;
; returns A = 0 we can move or A = 1 we are blocked
; X register is left intact
;---------------------------------------------------------------------------------
#region "CanMoveDown"
CanMoveDown
        lda SPRITE_POS_Y_DELTA,x                ; fetch Y delta for this sprite
        beq @downCheck                          ; only check if 0 - and flush with screen characters

        lda #0                                  ; else return with 0 - we can move
        rts

@downCheck
        lda SPRITE_POS_X_DELTA,x                ; Check X delta to see if we're flush on the X axis
        beq @downCheck2                         ; if not we need to check 2 characters
        
        ldy SPRITE_CHAR_POS_Y,x                 ; fetch character Y position and store it in Y
        iny                                     ; add 1

        lda SCREEN_LINE_OFFSET_TABLE_LO,y       ; fetch address for the screen line
        sta ZEROPAGE_POINTER_1
        lda SCREEN_LINE_OFFSET_TABLE_HI,y
        sta ZEROPAGE_POINTER_1 + 1
        
        ldy SPRITE_CHAR_POS_X,x                 ; fetch X character pos for sprite
        iny                                     ; increase by 1
        lda (ZEROPAGE_POINTER_1),y              ; fetch character at this position

        jsr TestBlocking
        bne @downBlocked

@downCheck2                                     ; Check character above the sprite
        ldy SPRITE_CHAR_POS_Y,x                 ; load the sprite Y character position
        iny                                     ; add 1

        lda SCREEN_LINE_OFFSET_TABLE_LO,y       ; fetch address for screen line
        sta ZEROPAGE_POINTER_1
        lda SCREEN_LINE_OFFSET_TABLE_HI,y
        sta ZEROPAGE_POINTER_1 + 1
        
        ldy SPRITE_CHAR_POS_X,x                 ; fetch X character position and store in Y

        lda (ZEROPAGE_POINTER_1),y              ; fetch character off screen and store in A
        
        jsr TestBlocking                        ; test for blocking
        bne @downBlocked

        lda#0                                   ; if not blocking return 0
        rts
@downBlocked
        lda #1                                  ; if blocked return 1
        rts
#endregion        
;=================================================================================
;                                                   TEST CHARACTER FOR BLOCKING
;=================================================================================
TestBlocking
        cmp #128                      ; is the character > 128?
        bpl @blocking
        
        lda #0
        rts
@blocking
        lda #1
        rts

