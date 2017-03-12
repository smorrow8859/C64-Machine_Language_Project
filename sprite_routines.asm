;===================================================================================================
;                                                                               SPRITE ROUTINES
;===================================================================================================
;                                                                              Peter 'Sig' Hewett
;                                                                                         - 2016
; Routines for more advanced handling and manipulation of sprites
;---------------------------------------------------------------------------------------------------

;===================================================================================================
;                                                                                 MOVE SPRITE LEFT
;===================================================================================================
; Moves a sprite left one pixel - using the whole screen (with the X extended bit)
; X = number of hardware sprite to move (0 - 7) - Register is left intact
;
; NOTE : to move a sprite multiple pixels, you call this multiple times. One day I might have a crack
;        at doing one for multiple pixels, but at this point I don't think I could do one that would
;        justify the extra code and provide a performance boost to make it worthwhile.
;---------------------------------------------------------------------------------------------------
; Fixed bug in the strange extended bit behavior.  Flipping the bit on negative flag sets it on $FF
; but also flips it on every other change from 128 ($80) to 255 ($FF) making it flicker.
; Checking instead for 0 corrects this.
;---------------------------------------------------------------------------------------------------
#region "MoveSpriteLeft"

;video - Commodore 64 Machine Language Project Week 3
;19:00

MoveSpriteLeft
        lda SPRITE_POS_X,x                      ; First check for 0 (NOT negative)
        bne @decNoChange                        ; branch if NOT 0

        dec SPRITE_POS_X,x                      ; Decrement Sprite X position by 1 (to $FF)

        lda BIT_TABLE,x                         ; fetch the bit needed to change for this sprite
        eor SPRITE_POS_X_EXTEND                 ; use it as a mask to flip the correct X extend bit 
        sta SPRITE_POS_X_EXTEND                 ; store teh data then save it in the VIC II register
        sta VIC_SPRITE_X_EXTEND                 ; $D011 - Sprite extended X bits (one bit per sprite)
        jmp @noChangeInExtendedFlag             ; Jump to saving the X position

@decNoChange                                    ; Not zero X so we decrement
        dec SPRITE_POS_X,x
@noChangeInExtendedFlag
        txa                                     ; copy X to the accumulator (sprite number)
        asl                                     ; shift it left (multiply by 2)
        tay                                     ; save it in Y (to calculate the register to save to)

        lda SPRITE_POS_X,x                      ; Load our variable saved X position
        sta VIC_SPRITE_X_POS,y                  ; save it in $D000 offset by Y to the correct VIC
                                                ; sprite register
        sta VIC_SPRITE_X_POS + 4,y

                                                ; Here we decrement the Sprite delta - we moved
                                                ; a pixel so the delta goes down by one
        dec SPRITE_POS_X_DELTA,x
        bmi @resetDelta                         ; test for change to negative
        rts                                     ; if delta is still > 0 we're done

@resetDelta                                     
        lda #$07                                ; if delta falls below 0 - represents the pixels in the character
        sta SPRITE_POS_X_DELTA,x                ; reset it to #$07 - one char
        dec SPRITE_CHAR_POS_X,x                 ; delta has reset - so decrement character position
        rts
     
#endregion


;===================================================================================================
;                                                                               MOVE SPRITE RIGHT
;===================================================================================================
; Moves a sprite right one pixel and adjusts the extended X bit if needed to carry to all the way
; across the screen.
; X = the number of the hardware sprite to move - this register is left intact
;
; NOTE : to move a sprite multiple pixels, this routine must be called multiple times
;---------------------------------------------------------------------------------------------------

;0 - Main Spelunker sprite
;2 - Overlay Spelunker sprite

;Example 
;       lda #2
;       jsr MoveSprite Right                    ;Will access the overlay sprite #2

#region "MoveSpriteRight"
MoveSpriteRight
        inc SPRITE_POS_X,x                      ; increase Sprite X position by 1
        lda SPRITE_POS_X,x                      ; load the sprite position
        bne @noChangeInExtendedFlag             ; if not #$00 then no change in x flag
        
        lda BIT_TABLE,x                         ; get the correct bit to set for this sprite
        eor SPRITE_POS_X_EXTEND                 ; eor in the extended bit (toggle it on or off)
        sta SPRITE_POS_X_EXTEND                 ; store the new flags
        sta VIC_SPRITE_X_EXTEND                 ; set it in the VIC register

@noChangeInExtendedFlag                          
        txa                                     ; transfer the sprite # to A
        asl                                     ; multiply it by 2
        tay                                     ; transfer the result to Y

        lda SPRITE_POS_X,x                      ; copy the new position to our variable
        sta VIC_SPRITE_X_POS,y                  ; update the correct X position register in the VIC
        sta VIC_SPRITE_X_POS + 4,y

                                                ; Our X position is now incremented, so delta also
                                                ; increases by 1
        inc SPRITE_POS_X_DELTA,x
        lda SPRITE_POS_X_DELTA,x
        cmp #$08                                ; if it's crossed over to 8, we reset it to 0
        beq @reset_delta
        rts                                     ; if it hasn't we're done
@reset_delta                                    
        lda #$00
        sta SPRITE_POS_X_DELTA,x                ; reset delta to 0 - this means we've crossed a
        inc SPRITE_CHAR_POS_X,x                 ; a character boundry, so increase our CHAR position
        rts

#endregion

;===================================================================================================
;                                                                                  MOVE SPRITE UP
;===================================================================================================
; Up and down have no special considerations to consider - they wrap at 255
; X = number of hardware sprite to move
;---------------------------------------------------------------------------------------------------
#region "MoveSpriteUp"

MoveSpriteUp
        dec SPRITE_POS_Y,x                      ; decrement the sprite position variable
        
        txa                                     ; copy the sprite number to A
        asl                                     ; multiply it by 2
        tay                                     ; transfer it to Y
        
        lda SPRITE_POS_Y,x                      ; load the sprite position for this sprite
        sta VIC_SPRITE_Y_POS,y                  ; send it to the correct VIC register - $D001 + y

                                                ; Y position has decreased, so our delta decreases
        dec SPRITE_POS_Y_DELTA,x
        bmi @reset_delta                        ; test to see if it drops to negative
        rts                                     ; if not we're done
@reset_delta
        lda #$07                                ; reset the delta to 0
        sta SPRITE_POS_Y_DELTA,x
        dec SPRITE_CHAR_POS_Y,x                 ; if delta resets, we've crossed a character border
        rts

#endregion

;===================================================================================================
;                                                                               MOVE SPRITE DOWN
;===================================================================================================
; Much the same
; X = number of hardware sprite to move
;---------------------------------------------------------------------------------------------------
#region "MoveSpriteDown"


MoveSpriteDown
        inc SPRITE_POS_Y,x                      ; increment the y pos variable for this sprite
                                                ; sans comments it looks kinda naked.......
        txa
        asl
        tay

        lda SPRITE_POS_Y,x
        sta VIC_SPRITE_Y_POS,y
        
        inc SPRITE_POS_Y_DELTA,x
        lda SPRITE_POS_Y_DELTA,x
        cmp #$08
        beq @reset_delta
        rts

@reset_delta
        lda #$00
        sta SPRITE_POS_Y_DELTA,x
        inc SPRITE_CHAR_POS_Y,x
        rts

#endregion

;===================================================================================================
;                                                                             SPRITE TO CHAR POS
;===================================================================================================
; Puts a sprite at the position of character X Y. Calculates the proper sprite coords from the
; screen memory position then sets it there directly.
; The primary use of this is the inital positioning of any sprite as it will align it with the
; proper delta set up.
;
; PARAM 1 = Character x pos (column)
; PARAM 2 = Character y pos (row)
; X = sprite number
;---------------------------------------------------------------------------------------------------
#region "SpriteToCharPos"

;video - Commodore 64 Machine Language Project Week 3
;21:50

SpriteToCharPos
        lda BIT_TABLE,x                 ; Lookup the bit for this sprite number (0-7)
        eor #$ff                        ; flip all bits (invert the byte %0001 would become %1110)
        and SPRITE_POS_X_EXTEND         ; mask out the X extend bit for this sprite
        sta SPRITE_POS_X_EXTEND         ; store the result back - we've erased just this sprites bit
        sta VIC_SPRITE_X_EXTEND         ; store this in the VIC register for extended X bits

        lda PARAM1                      ; load the X pos in character coords (the column)
        sta SPRITE_CHAR_POS_X,x         ; store it in the character X position variable
        cmp #30                         ; if X is less than 30, no need set the extended bit
        bcc @noExtendedX
        
        lda BIT_TABLE,x                 ; look up the the bit for this sprite number
        ora SPRITE_POS_X_EXTEND         ; OR in the X extend values - we have set the correct bit
        sta SPRITE_POS_X_EXTEND         ; Store the results back in the X extend variable
        sta VIC_SPRITE_X_EXTEND         ; and the VIC X extend register

@noExtendedX
                                        ; Setup our Y register so we transfer X/Y values to the
                                        ; correct VIC register for this sprite
        txa                             ; first, transfer the sprite number to A
        asl                             ; multiply it by 2 (shift left)
        tay                             ; then store it in Y 
                                        ; (note : see how VIC sprite pos registers are ordered
                                        ;  to understand why I'm doing this)

        lda PARAM1                      ; load in the X Char position
        asl                             ; 3 x shift left = multiplication by 8
        asl
        asl
        clc                             
        adc #24 - SPRITE_DELTA_OFFSET_X ; add the edge of screen (24) minus the delta offset
                                        ; to the rough center 8 pixels (1 char) of the sprite

        sta SPRITE_POS_X,x              ; save in the correct sprite pos x variable
        sta VIC_SPRITE_X_POS,y          ; save in the correct VIC sprite pos register
        sta VIC_SPRITE_X_POS + 4,y


        lda PARAM2                      ; load in the y char position (rows)
        sta SPRITE_CHAR_POS_Y,x         ; store it in the character y pos for this sprite
        asl                             ; 3 x shift left = multiplication by 8
        asl
        asl
        clc
        adc #50 - SPRITE_DELTA_OFFSET_Y ; add top edge of screen (50) minus the delta offset
        sta SPRITE_POS_Y,x              ; store in the correct sprite pos y variable
        sta VIC_SPRITE_Y_POS,y          ; and the correct VIC sprite pos register

        lda #0
        sta SPRITE_POS_X_DELTA,x        ;set both x and y delta values to 0 - we are aligned
        sta SPRITE_POS_Y_DELTA,x        ;on a character border (for the purposes of collisions)
        rts

#endregion

