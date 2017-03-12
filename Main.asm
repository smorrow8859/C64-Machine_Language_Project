
;===================================================================================================
; C64 Brain Assembly Language Project Framework 1.1
; 2016 - Peter 'Sig' Hewett aka RetroRomIcon
;===================================================================================================
#region "ChangeLog"
; Changelog
; 1.1 - changed custom charset to reflect the layout in 'Bear Essentials'
;     - changed chars so 0-9 A-Z start at 0 so displaying hex (0-F) debug
;       info / scores / numbers will be easier
;     - removed many 'tutorial comments' and left only notes
;     - exported VIC and register defines to a seperate file
;     - added screen clear with color and character to clear with
;     - added 'raster time' indicator (yellow bar)

; 1.2   Added (DisplayText) - display a 0 truncated string at X/Y with linebreak and color
;       Added (DisplayByte) - display the contents of a byte in hex at X/Y
;       Changed Character A-Z 0-9 back to normal, because I'm stupid
;       Added Joystick Read and basic Move Player (UpdateSprites)
;       Added Sprite move u/d/l/r with extended X bit updating
;       (MoveSpriteLeft, MoveSpriteRight), etc.        
;       Added Handling variables for all hardware sprites
;       Moved routines to 'core' and 'sprites' asm files to keep things neat
;
; 1.3   Setup multicolor sprites with killbot as SPRITE 0 and 1
;       Expanded ANIM_FRAME and SPRITE_DIRECTION variables to include all 8 sprites
;       (AnimTest2)
;       Cleaned up code from live session so 'AnimTest2' works for all sprites using X register
;       Added 'standing still' to AnimTest2 if direction = 0
;
;       Extended Debug Panel to include char pos and delta x y values
;       Added SPRITE_CHAR_POS_X/Y and SPRITE_POS_X/Y_DELTA variables for all sprites
;       Extended MoveSprite routines to include delta and character coord updates
;       Sprite loading directly from the sprite editor - no more exporting to binary needed
;       Character set loading directly from the character editor - no more exporting to binary
;       Added loadpointer macro
;       Added (SpriteToCharPos) to set a sprite to character screen coords
;       Added DrawVLine and DrawHLine routines to draw simple character lines
;       Added CanMoveLeft and CanMoveRight to test for blocking characters
;       Added CanMoveUp and CanMoveDown to test for blocking characters
;       Added SPRITE_DELTA_TRIM_X/Y variable to fine tune a sprite to background collision
;       Added TestBlock - test for characters that block (128 - 255)
;       Bounds check on left/right of visible screen through character collision checks
;       Added raster.asm with routines to initialize and remove raster irq chains.
;       Setup a basic raster interrupt chain for the top of screen to handle joystick and
;         and timers, and another at the start of the 'score panel'

;       Did some cleanup implementing CBM PRG Studio's #region / #endregion to collapse code
;       
;       TODO - track down and fix a few small 'glitch exceptions' on collisions on up/down
;              while using trim to adjust for wider sprites - though on a platform style game
;              these may not be an issue
             
;       Demo - Extended animation over 8 sprite images under joy control
;       Demo - Sprite to character collisons using wall borders and a simple platform
;       Demo - Raster color change to show scorboard raster interrupt position

;-------------------------------------------------------------------------------------------------
#endregion
;===============================================================================
;                                                                   DIRECTIVES
;===============================================================================
Operator Calc        ; IMPORTANT - calculations are made BEFORE hi/lo bytes
                     ;             in precidence (for expressions and tables)
;===============================================================================
;                                                                   DEFINITIONS
;===============================================================================
IncAsm "VICII.asm"                      ; VICII register includes
IncAsm "macros.asm"                     ; macro includes
;===============================================================================
;===============================================================================
;                                                                     CONSTANTS
;===============================================================================
; Defining things as constants, as above, makes things both easier to read
; and also makes things easier to change.
;--------------------------------------------------------------------------------
#region "Constants"

SCREEN_MEM = $4000                   ; Bank 1 - Screen 0
COLOR_MEM  = $D800                   ; Color mem never changes
CHAR_MEM   = $4800                   ; Base of character set memory
SPRITE_MEM = $5000                   ; Base of sprite memory

COLOR_DIFF = COLOR_MEM - SCREEN_MEM  ; difference between color and screen ram
                                     ; a workaround for CBM PRG STUDIOs poor
                                     ; expression handling

SPRITE_POINTER_BASE = SCREEN_MEM + $3f8 ; last 8 bytes of screen mem

SPRITE_BASE = 64                        ; the pointer to the first image

SPRITE_0_PTR = SPRITE_POINTER_BASE + 0  ; Sprite pointers
SPRITE_1_PTR = SPRITE_POINTER_BASE + 1
SPRITE_2_PTR = SPRITE_POINTER_BASE + 2
SPRITE_3_PTR = SPRITE_POINTER_BASE + 3
SPRITE_4_PTR = SPRITE_POINTER_BASE + 4
SPRITE_5_PTR = SPRITE_POINTER_BASE + 5
SPRITE_6_PTR = SPRITE_POINTER_BASE + 6
SPRITE_7_PTR = SPRITE_POINTER_BASE + 7

SPRITE_DELTA_OFFSET_X = 8               ; Offset from SPRITE coords to Delta Char coords
SPRITE_DELTA_OFFSET_Y = 11              ; approx the center of the sprite


NUMBER_OF_SPRITES_DIV_4 = 3           ; This is for my personal version, which
                                      ; loads sprites and characters under IO ROM

#endregion

;===============================================================================
;                                                            ZERO PAGE VARIABLES
;===============================================================================
#region "ZeroPage"
PARAM1 = $03                 ; These will be used to pass parameters to routines
PARAM2 = $04                 ; when you can't use registers or other reasons
PARAM3 = $05                            
PARAM4 = $06                 ; essentially, think of these as extra data registers
PARAM5 = $07

ZEROPAGE_POINTER_1 = $17     ; Similar only for pointers that hold a word long address
ZEROPAGE_POINTER_2 = $19
ZEROPAGE_POINTER_3 = $21
ZEROPAGE_POINTER_4 = $23

#endregion

;===============================================================================
;                                                           BASIC KICKSTART
;===============================================================================
KICKSTART
; Sys call to start the program - 10 SYS (2064)

*=$0801

        BYTE $0E,$08,$0A,$00,$9E,$20,$28,$32,$30,$36,$34,$29,$00,$00,$00


;==============================================================================
;                                                              PROGRAM START
;==============================================================================
*=$0810

PRG_START

        lda #0                          ; Turn off sprites 
        sta VIC_SPRITE_ENABLE

        lda VIC_SCREEN_CONTROL          ; turn screen off with bit 4
        and #%11101111                  ; mask out bit 4 - Screen on/off
        sta VIC_SCREEN_CONTROL          ; save back - setting bit 4 to off
        ;-----------------------------------------------------------------------
        ;                                                       VIC BANK SETUP
        ;-----------------------------------------------------------------------
#region "VIC Setup"
        ; To set the VIC bank we have to change the first 2 bits in the
        ; CIA 2 register. So we want to be careful and only change the
        ; bits we need to.

        lda VIC_BANK            ; Fetch the status of CIA 2 ($DD00)
        and #%11111100          ; mask for bits 2-8
        ora #%00000010          ; the first 2 bits are your desired VIC bank value
                                ; In this case bank 1 ($4000 - $7FFF)
        sta VIC_BANK
        ;-----------------------------------------------------------------------
        ;                                          CHARACTER SET AND SCREEN MEM
        ;-----------------------------------------------------------------------
        ; Within the VIC Bank we can set where we want our screen and character
        ; set memory to be using the VIC_MEMORY_CONTROL at $D018
        ; It is important to note that the values given are RELATIVE to the start
        ; address of the VIC bank you are using.
       
        lda #%00000010   ; bits 1-3 (001) = character memory 2 : $0800 - $0FFF
                         ; bits 4-7 (000) = screen memory 0 : $0000 - $03FF
                         ; this leaves screen 1 intact at $0400 - $07ff

        sta VIC_MEMORY_CONTROL

        ; Because these are RELATIVE to the VIC banks base address (Bank 1 = $4000)
        ; this gives us a base screen memory address of $4000 and a base
        ; character set memory of $4800
        ; 
        ; Sprite pointers are the last 8 bytes of screen memory (25 * 40 = 1000 and
        ; yet each screen reserves 1024 bytes). So Sprite pointers start at
        ; $4000 + $3f8.

        ; Sprite data starts at $5000 - giving the initial image a pointer value of 64
        ; (The sprite data starts at Bank Address + $1000.  $1000 / 64 = 64)
#endregion        
        ;-----------------------------------------------------------------------
        ;                                                       SYSTEM SETUP
        ;-----------------------------------------------------------------------
#region "System Setup"
System_Setup

        ; Here is where I copy my charset and sprite data if using Bank 3 to under
        ; the IO ROM. I'll leave this as a stub in case it comes up later.
 
        sei           

        ; Here you would load and store the Processor Port ($0001), then use 
        ; it to turn off LORAM (BASIC), HIRAM (KERNAL), CHAREN (CHARACTER ROM)
        ; then use a routine to copy your sprite and character mem under there
        ; before restoring the original value of $0001 and turning interrupts
        ; back on.

        cli
#endregion
        ;-----------------------------------------------------------------------
        ;                                                       SCREEN SETUP
        ;------------------------------------------------------------------------
#region "Screen Setup"
Screen_Setup
        lda #COLOR_BLACK
        sta VIC_BORDER_COLOR            ; Set border and background to black
        sta VIC_BACKGROUND_COLOR        

        lda #$40                        ; use character #$40 as fill character (bricks)
        ldy #COLOR_BLUE                 ; use blue as fill color 
        jsr ClearScreen                 ; clear screen


                                        ; Display a little message to test our 
                                        ; custom character set and text display routines

                                        ; Setup for the DisplayText routine
      ;  lda #<VERSION_TEXT              ; Loading a pointer to TEST_TEXT - load the low byte 
      ;  sta ZEROPAGE_POINTER_1          ; or the address into the pointer variable
      ;  lda #>VERSION_TEXT              ; Then the high byte to complete the one word address
       ; sta ZEROPAGE_POINTER_1 + 1      ; (just in case someone didn't know what that was)
       
                                         ; loadPointer Macro - you need never type all that out
                                         ; again

        loadPointer ZEROPAGE_POINTER_1, VERSION_TEXT

        ;---------------------------------------------------------------------------------
        ;                       Draw the walls around the screen
        ;---------------------------------------------------------------------------------


        lda #1                          
        sta PARAM1                      ; PARAM1 and PARAM2 hold X and Y screen character coords
        sta PARAM2                      ; To write the text at
        lda #COLOR_WHITE                ; PARAM3 hold the color 
        sta PARAM3

        jsr DisplayText
        ;--------------------------------------------------------- DEBUG TEXT CONSOLE
                                        ; Display the little debug panel showing
                                        ; sprite pos and extended X bit status

                                        ; ZEROPAGE_POINTER_1 contains the address to the text

        ;lda #<CONSOLE_TEXT              ; Load the pointer to the text low byte
        ;sta ZEROPAGE_POINTER_1
        ;lda #>CONSOLE_TEXT              ; Load the pointer to the text high byte
        ;sta ZEROPAGE_POINTER_1 + 1

        loadpointer ZEROPAGE_POINTER_1, CONSOLE_TEXT

        lda #0                          ; PARAM1 contains X screen coord (column)
        sta PARAM1
        lda #20                         ; PARAM2 contains Y screen coord (row)
        sta PARAM2
        lda #COLOR_WHITE                ; PARAM3 contains the color to use
        sta PARAM3
        jsr DisplayText                 ; Then we display the text - Messages for stats

        ;------------------------------------------------------------ WALLS
        ; Draw walls on left and right of the screen
                                
        lda #0                  ; X character coord
        sta PARAM1
        lda #3                  ; Y character coord
        sta PARAM2
        lda #19                ;  end Y character coord
        sta PARAM3
        lda #$c0                ; character to draw
        sta PARAM4
        lda #COLOR_YELLOW       ; color to use
        sta PARAM5
        jsr DrawVLine
                                ; Routine doesn't trash PARAM variables so with care
                                ; you can shorten your workload
        lda #39                 ; change X to 39 - other side of the screen
        sta PARAM1
        jsr DrawVLine           ; draw the same line over there
        
                                ; setup for horizontal lines
        lda #1                  ; Top Wall
        sta PARAM1              ; starting at X 1
        lda #3                 
        sta PARAM2              ; starting at Y 3
        lda #39         
        sta PARAM3              ; ending at X 39 - Draws over to
        jsr DrawHLine

                                ; Bottom Wall (Floor)
        lda #0
        sta PARAM1
        lda #18                 
        sta PARAM2              ; starting at Y 18 - BOTTOM LINE
        lda #40
        sta PARAM3
        jsr DrawHLine

        lda #15
        sta PARAM1
        lda #11
        sta PARAM2
        lda #20
        sta PARAM3
        jsr DrawHLine

#endregion
        ;------------------------------------------------------------------------
        ;                                                       SPRITE SETUP
        ;------------------------------------------------------------------------
#region "Sprite Setup"
        ;-------------------------------------------- SETUP AND DISPLAY TEST SPRITE 0
;        lda #0
;        sta VIC_SPRITE_MULTICOLOR       ; set all sprites to single color for now

        lda #%00000011                  ; make SPRITE 0 - 1 (KillBot) multicolor
        sta VIC_SPRITE_MULTICOLOR 

        lda #COLOR_LTRED                  ; set shared sprite multicolor 1
        sta VIC_SPRITE_MULTICOLOR_1
        lda #COLOR_YELLOW
        sta VIC_SPRITE_MULTICOLOR_2     ; set shared sprite multicolor 2

        lda #COLOR_WHITE
        sta VIC_SPRITE_COLOR            ; set sprite 0 color to White

        ;------------------------------------------------ SETUP SPRITE 0 START POSITION
        lda #0
        sta VIC_SPRITE_X_EXTEND         ; clear extended X bits

;        no more setting sprites up like this
;
;        lda #100
;        sta VIC_SPRITE_X_POS            ; display at 100,100 sprite coords for now
;        sta VIC_SPRITE_Y_POS
;        sta SPRITE_POS_X
;        sta SPRITE_POS_Y
;
        ; We are now using a system that tracks the sprites position in character coords
        ; on the screen. To avoid many costly calculations every frame, we set the sprite 
        ; to initally be on a character border, and increase/decrease it's delta values
        ; and character coords as it moves. This way we need only do these calculations
        ; once.
        ;
        ; To initially place a sprite we use 'SpriteToCharPos'

        lda #10
        sta PARAM1              ; Character column 10 (X coord)
        sta PARAM2              ; Character row 10 (Y coord)

        ldx #0                  ; Sprite # in X (0)
        jsr SpriteToCharPos
        ldx #2
        jsr SpriteToCharPos

        lda #3
        sta SPRITE_DELTA_TRIM_X         ; Trim delta for better collisions
        ;-----------------------------------------------------------------------
        ;Running animation sprite
        ;SPRITE_POS_X + 0 - joystick sprite
        ;SPRITE_POS_X + 1 - animation sprite
        ;SPRITE_POS_X + 2 - joystick background shape

        lda #100                        ; Store X/Y coords of Sprite 1 at 100,200
        sta VIC_SPRITE_X_POS         ; Add 2 to arrive at $D002 - Sprite 1 X - $D002
        ;sta VIC_SPRITE_X_POS + 4
        ;sta SPRITE_POS_X + 1            ; Store in our X pos variable
        ;sta SPRITE_POS_X + 2
        ;sta VIC_SPRITE_X_POS + 4

        lda #170
        sta VIC_SPRITE_Y_POS         ; Store 200 in the Y variable and the VIC
        ;sta VIC_SPRITE_Y_POS + 4
        ;sta SPRITE_POS_Y + 1
        ;sta SPRITE_POS_Y + 2
        ;sta VIC_SPRITE_Y_POS + 4
        

;        lda #COLOR_WHITE                 ; Set sprite 0 to white
;        sta VIC_SPRITE_COLOR

        lda #COLOR_CYAN                 ; Set sprite 1 to cyan
        sta VIC_SPRITE_COLOR + 1        ; Sprite color registers run concurrently 0-8

        lda #SPRITE_BASE            ; Take our first sprite image (Kilbot)
        sta SPRITE_0_PTR                ; store it in the pointer for sprite 0

;        lda #SPRITE_BASE + 32           ; Take sprite image 5 (Running killbot)
;        sta SPRITE_1_PTR
        ;lda #SPRITE_BASE + 32
        ;sta SPRITE_2_PTR

        lda #$00
        sta SPRITE_DIRECTION + 1        ; start running dude moving right
        lda #$00
        ;sta ANIM_FRAME + 1              ; starting sprite image for runner (4)

        lda #%00000111                  ; Turn on sprites 0 and 1
        sta VIC_SPRITE_ENABLE 

        jsr DisplayInfoNow              ; a label to update the joystick test info
#endregion        
        ;------------------------------------------------------------------------
        ;                                                       RASTER SETUP
        jsr WaitFrame
        jsr InitRasterIRQ
        jsr WaitFrame

        
        lda VIC_SCREEN_CONTROL           ;$D011
        and #%11101111                  ; mask for bit 4 - Screen on/off
        ora #%00010000                  ; or in bit 4 - turn screen on
        sta VIC_SCREEN_CONTROL
        ;=======================================================================
        ;                                                           MAIN LOOP
        ;=======================================================================
        ; The main loop of the program - timed to the verticle blanking period
        ;-----------------------------------------------------------------------
MainLoop
        jsr WaitFrame                   ; wait for the vertical blank period

        lda #COLOR_YELLOW               ; Raster time indicator - turn the border yellow
        sta VIC_BORDER_COLOR            ; before the main loop starts

;                                          These are now under raster interrupt
;        jsr UpdateTimers                ; update the basic timers
;        jsr ReadJoystick                ; read the joystick
;        jsr JoyButton                   ; read the joystick button

;        jsr UpdateScroll

        jsr UpdateSprites               ; update the sprites

        jsr DisplayInfo                 ; Display simple debug info
        
        lda #COLOR_BLACK                ; Restore the border to black - this gives a visual
        sta VIC_BORDER_COLOR            ; on how much 'raster time' you have to work with

        jmp MainLoop

        ;=======================================================================
        ;=======================================================================
        ;                                                             ROUTINES
        ;=======================================================================
        incAsm "raster.asm"                         ; raster interrupts
        incAsm "core_routines.asm"                  ; core framework routines
        incAsm "sprite_routines.asm"                ; sprite handling
        incAsm "collision_routines.asm"             ; sprite collision routines
        incAsm "screen_routines.asm"                ; screen drawing and handling

        ;-----------------------------------------------------------------------
        ;                                                         UPDATE PLAYER
        ;-----------------------------------------------------------------------
        ; Update the Player Sprite using a joystick read and some simple sprite
        ; anim tests.
        ;------------------------------------------------------------------------
#region "UpdateSprites"
UpdateSprites
        ;------------------------------------------------------------------------
        ;                       SPRITE 0 DEMO - JOYSTICK AND 2 FRAME FLIP ANIM
 
                                        ; Fetch Joystick X and move horizontally

        ldx #$00                        ; Sprite 0 in X
        lda JOY_X                       ; fetch Joystick X position
        sta SPRITE_DIRECTION,x          ; store this in SPRITE_DIRECTION for sprite 0
                               
                                        ; now we test this data from the joystick. it can only
                                        ; be -1 o or 1 for left - still or right

        cmp #0                          ; 0 = no input in X axis
        beq @testUpDown                 ; so we can go on to test the Y axis


        bmi @moveLeft                   ; Our joystick reader treats this as a signed bytes
                                        ; so we use BMI (BRanch Minus) rather than BCC or BCS
        jsr CanMoveRight
        bne @testUpDown
        
        ldx #0                          ; Show main Spelunker sprite
        jsr MoveSpriteRight             ; Joystick X positive - move right
        ldx #2                          ; Show overlay Spelunker sprite #2
        jsr MoveSpriteRight

        jmp @testUpDown

@moveLeft
        ldx #0
        jsr CanMoveLeft                 ; check to see if we can move left
        bne @testUpDown                 ; if blocked - no can move that way

        ldx #0
        jsr MoveSpriteLeft              ; Joystick X negative - move left
        ;ldx #2
        ;jsr MoveSpriteLeft 


@testUpDown
                                        ; Now that we're using a delta system, I can't do a simple
                                        ; update by adding the Joystick Y axis to the VIC_SPRITE_Y
                                        ; we MUST go through the SpriteMove routines.

        lda JOY_Y                       ; Fetch the Joystick Y axis value (-1 0 or 1)
        cmp #$00                        ; if 0 then it's not moved - so we're done
        beq @done

        bmi @moveUp                     ; if Joystick is negative, then we move up

        ldx #0
        jsr CanMoveDown
        bne @done
        jsr MoveSpriteDown              ; if it's positive, we move it down
        jmp @done                       ; and move on to the animation

@moveUp
        ldx #0
        jsr CanMoveUp
        bne @done
        jsr MoveSpriteUp 


@done 
                                       ; UPDATE SIMPLE ANIMATION
        
        jsr AnimTest2
        ;jsr Runningman


#endregion

        ;------------------------------------------------------------------
        ;                               END RUNNING MAN
        ;------------------------------------------------------------------

        ;-----------------------------------------------------------------------
        ;                                                         DISPLAY INFO
        ;-----------------------------------------------------------------------
        ; Updates the info in our Debug 'console'
        ;-----------------------------------------------------------------------
#region "DisplayInfo"
DisplayInfo
        lda JOY_X                       ; only display if changed position
        bne DisplayInfoNow                 
        lda JOY_Y
        bne DisplayInfoNow
        rts
        ;----------------------------------- DEBUGGING INFO - Screen pos and extended bit
DisplayInfoNow

;@displayPos                                    ; Display the debug data
        lda SPRITE_POS_X                        ; Byte to be displayed
        ldx #20                                 ; Y position to display at (row)                                 
        ldy #7                                  ; X position to display at (column)
        jsr DisplayByte                         ; Display the byte

        lda SPRITE_POS_Y
        ldx #20
        ldy #18
        jsr DisplayByte

        lda ANIM_FRAME,x                  ; Address of data to display
        ldx #21                         ; Y position
        ldy #28                         ; X position
        jsr DisplayByte                 ; Call the display routine

                                                ; check the extended x bit
        lda SPRITE_POS_X_EXTEND
        and #$01                                ; mask bit one
        bne @extend                             ; if it's set, display an *

        lda #' '                                ; if not, display a space
        sta SCREEN_MEM + #810
        lda #COLOR_WHITE
        sta COLOR_MEM + #810
        jmp @displayCharCoords                  ; display the next bunch of info
        
@extend

        lda #'*'
        sta SCREEN_MEM + #810
        lda #COLOR_WHITE
        sta COLOR_MEM + #810

        ;------------------------------------------ DISPLAY CHAR X and Y POS AND DELTA
@displayCharCoords
        lda SPRITE_CHAR_POS_X           ; Address of data to display
        ldx #20                         ; Y position
        ldy #28                         ; X position
        jsr DisplayByte                 ; Call the display routine

        lda SPRITE_CHAR_POS_Y           ; Address of data to display
        ldx #20                         ; Y position
        ldy #37                         ; X position
        jsr DisplayByte                 ; Call the display routine

        lda SPRITE_POS_X_DELTA          ; Address of data to display
        ldx #21                         ; Y position
        ldy #7                          ; X position
        jsr DisplayByte                 ; Call the display routine

        lda SPRITE_POS_Y_DELTA          ; Address of data to display
        ldx #21                         ; Y position
        ldy #18                         ; X position
        jsr DisplayByte                 ; Call the display routine

        lda ANIM_FRAME                  ; Address of data to display
        ldx #21                         ; Y position
        ldy #28                         ; X position
        jsr DisplayByte                 ; Call the display routine

        lda JOY_X                       ; Address of data to display
        ldx #21                         ; Y position
        ldy #37                         ; X position
        jsr DisplayByte                 ; Call the display routine

        rts

#endregion

;delta_debug
;        ;------------ DEBUG MY DELTA and CHAR POS - remove later
;        ; change color of character one row down from sprite
;        
;        ldx SPRITE_CHAR_POS_Y                    ; sprite 0's char pos Y
;        inx                                      ; inc by 1 (character UNDER)

;        lda SCREEN_LINE_OFFSET_TABLE_LO,x       ; fetch the Y pos line address
;        sta ZEROPAGE_POINTER_1
;        lda SCREEN_LINE_OFFSET_TABLE_HI,x
;        sta ZEROPAGE_POINTER_1 + 1

;        clc
;        adc #>COLOR_DIFF                        ; add color diff to get color ram
;        sta ZEROPAGE_POINTER_1 + 1
;        
;        ldy SPRITE_CHAR_POS_X                   ; put the sprite character X in Y
;        lda #COLOR_LTRED                
;        sta (ZEROPAGE_POINTER_1),y              ; use that offset to change the character to red
;        rts
;        ;--------------------------------------------------------------------

        ;-----------------------------------------------------------------------
        ;                       ROBOT TIMER - ANIM TEST
        ;-----------------------------------------------------------------------
        ; A basic test - flip the sprite back and forth between images 0 and 1
        ; this is hardcoded to sprite 0 atm.
        ;-----------------------------------------------------------------------

AnimTest
;        lda SLOW_TIMER                  ; Take the value of slow timer
;        and #$01                        ; check the value of the first bit
;        beq @frame1                     ; if it's 0, use the first frame
;        lda #SPRITE_BASE + 2                ; Take our first sprite image
;        sta SPRITE_0_PTR                ; store it in the pointer for sprite 0
;        rts
;        
;@frame1                                 ; if it's 1, use the second frame
;        lda #SPRITE_BASE + 4             ; Take our second sprite image
;        sta SPRITE_0_PTR                ; store it in the pointer for sprite 0

;        rts

;--------------------------------------------------------------------------------
;                                                                    ANIM TEST 2
;--------------------------------------------------------------------------------
; Slightly more complex animation flipping between 4 images with both left and
; right animations for a running character.
;
; X contains the sprite number you want to animate.
;
; NOTE this is hardcoded to sprite images 0 to 20 - which contain 'killbot'
;--------------------------------------------------------------------------------

;Explained by Siggy in video "Commodore 64 Machine Language Project Week 2"
; 2:35:34
AnimTest2
                                   ; Use TIMER to update the animation.
        lda TIMER                  ; Every frame is too fast - and slow timer won't
                                   ; generate a 'pulse' (it stays on then off) - so we check
                                   ; BOTH  bits 1 and 2 (3) for a quick regular pulse every
                                   ; couple of frames
        and #$07                   ; slow down the animation a bit
        beq @updateAnimation
        rts

; 2:37:51
@updateAnimation

        inc ANIM_FRAME              ; move to the next anim image

        lda SPRITE_DIRECTION        ; see if we're standing still - equal would be 0
        bmi @movingLeft

;Moving right   
        clc                         ; Sprite moving right - use frames 3-7
        lda ANIM_FRAME
        cmp #6                                   
        bcc @updateAnim
        lda #0                      ; reset back to start frame
        sta ANIM_FRAME
        jmp @updateAnim

        
@movingLeft
        inc ANIM_LEFT
                                    ; Sprite moving left - use frames 9-12
        lda ANIM_LEFT            
        cmp #19                     ; Check to make sure the anim frame isn't = 20
        bcs @resetLeft              ; Reset to the start frame if it's overrun
        jmp @updateAnim
;                

@resetLeft
        lda #14                     ; reset to frame start if it overruns
        sta ANIM_LEFT
        jmp @updateAnim
                                    ; Update the displayed frame
@updateAnim

        clc
        adc #SPRITE_BASE           ; pointer = SPRITE_BASE + FRAME #
        sta SPRITE_0_PTR         ; store new image pointer the correct sprite pointer1
        rts


Runningman
        ;------------------------------------------------------------------
        ;                               SPRITE 1 DEMO - RUNNING MAN
        ;------------------------------------------------------------------
        
        ldx #01                         ; Set the X register to the sprite number [1]
        lda SPRITE_DIRECTION,x          ; Check direction
        bmi @moveLeft1
                                        ; Moving Right
        ldx #$01                        ; Load the sprite number in x (Hardware Sprite 1)
        jsr MoveSpriteRight             ; Move it right - this routine leaves X register intact
                                        
                                        ; RIGHT SCREEN BORDER CHECK
                                        ; Check for edge of screen ($53 with X extend set)
        lda BIT_TABLE,x                 ; get the relevent bit for this Sprite (Bit 2)
        and SPRITE_POS_X_EXTEND         ; if I and these together - it will return 1 extend is set
        beq @updateAnim                 ; if it returns 0 - there's no need for further checks

        clc                             ; clear carry flag
        lda SPRITE_POS_X,x              ; Get the sprites X position
        cmp #$53                        ; Sprite coords for just off edge of screen
        bcc @updateAnim                 ; if it's less than $53, we're done

        lda #-1
        sta SPRITE_DIRECTION,x           ; Set direction to left

        jmp @updateAnim


@moveLeft1
        ldx #$01                        ; Load the sprite number in X
        jsr MoveSpriteLeft              ; Move sprite one pixel left

                                        ; LEFT SCREEN BORDER CHECK
                                        ; Check for edge of screen ($05 with X extend cleared)
        lda BIT_TABLE,x                 ; Get the relevent bit for the sprite 
        and SPRITE_POS_X_EXTEND         ; and it with the extend bit data
        bne @updateAnim                 ; if it's set, we're around the right 1/3 of the screen 
     
        clc
        lda SPRITE_POS_X,x              ; check if sprite pos is less than $05
        cmp #$05        
        bcs @updateAnim                 ; if it's greater than, we're done
        
        lda #1
        sta SPRITE_DIRECTION,x          ; set Direction to Right


@updateAnim
        ;jsr AnimTest2

        rts

;===============================================================================
;                                                       CHARSET AND SPRITE DATA
;===============================================================================
; Charset and Sprite data directly loaded here.

DATA_INCLUDES
; CHARACTER SET SETUP
;--------------------
; Going with the 'Bear Essentials' model would be :
;
; 000 - 063    Normal font (letters / numbers / punctuation, sprite will pass over)
; 064 - 127    Backgrounds (sprite will pass over)
; 128 - 143    Collapsing platforms (deteriorate and eventually disappear when stood on)
; 144 - 153    Conveyors (move the character left or right when stood on)
; 154 - 191    Semi solid platforms (can be stood on, but can jump and walk through)
; 192 - 239    Solid platforms (cannot pass through)
; 240 - 255    Death (spikes etc)
;
*=$4800
incbin "Chars.cst",0,255
*=$5000
;incbin "killbot.spt",1,20,true          ; Killbot -     SPRITES 0 - 19
;incbin "Sprites.spt",1,4,true           ; Waving guy -  SPRITES 20 - 23
;incbin "RunningMan.spt",1,4,true        ; Running guy - SPRITES 24 -31
incbin "spelunk7.spt",1,59,true         ; Spelunker   - SPRITES 32 - 59

;-------------------------------------------------------------------------------
;                                                               PROGRAM DATA
;-------------------------------------------------------------------------------
; All program data and variables fall in after the Sprite data
                                                        ; Timer Variables

TIMER                                                   ; Fast timer updates every frame
        Byte $0
SLOW_TIMER                                              ; Slow timer updates every 16th frame
        Byte $0

VERSION_TEXT
        byte 'SPELUNKER v1.3',0

CONSOLE_TEXT
        byte ' xpos:$     ypos:$    chrx:$   chry:$   / dltx:$     dlty:$                      ',0
;---------------------------------------------------------------------------------------------------
;                                                                                       JOYSTICK
JOY_X                           ; current positon of Joystick(2)
        byte $00                ; -1 0 or +1
JOY_Y
        byte $00                ; -1 0 or +1

BUTTON_PRESSED                  ; holds 1 when the button is held down
        byte $00
BUTTON_ACTION                   ; holds 1 when a single press is made (button released)
        byte $00
;---------------------------------------------------------------------------------------------------
;                                                                                       SPRITES

SPRITE_POS_X                                            ; Hardware sprite X position
        byte $00,$00,$00,$00,$00,$00,$00,$00
SPRITE_POS_X_DELTA                                      ; Delta X positon (0-7) - position within
        byte $00,$00,$00,$00,$00,$00,$00,$00            ; a character
SPRITE_CHAR_POS_X                                       ; Char pos X - sprite position in character
        byte $00,$00,$00,$00,$00,$00,$00,$00            ; coords (0-40)
SPRITE_DELTA_TRIM_X
        byte $00,$00,$00,$00,$00,$00,$00,$00            ; Trim delta for better collisions

SPRITE_POS_X_EXTEND                                     ; extended flag for X positon > 255
        byte $00                                        ; bits 0-7 correspond to sprite numbers


SPRITE_POS_Y                                            ; Hardware sprite Y position
        byte $00,$00,$00,$00,$00,$00,$00,$00
SPRITE_POS_Y_DELTA
        byte $00,$00,$00,$00,$00,$00,$00,$00
SPRITE_CHAR_POS_Y
        byte $00,$00,$00,$00,$00,$00,$00,$00


                                                ; Some variables for the anim demo - direction the
                                                ; sprite is moving (left or right) and the current frame
SPRITE_DIRECTION                                                
        byte $00,$00,$00,$00,$00,$00,$00,$00    ; Direction of the sprite (-1 0 1)
ANIM_FRAME
        byte $00,$00,$00,$00,$00,$00,$00,$00    ; Current animation frame

ANIM_LEFT byte $E
;---------------------------------------------------------------------------------------------------
; Bit Table
; Take a value from 0 to 7 and return it's bit value
BIT_TABLE
        byte 1,2,4,8,16,32,64,128
;---------------------------------------------------------------------------------------------------
; Screen Line Offset Tables
; Query a line with lda (POINTER TO TABLE),x (where x holds the line number)
; and it will return the screen address for that line

; C64 PRG STUDIO has a lack of expression support that makes creating some tables very problematic
; Be aware that you can only use ONE expression after a defined constant, no braces, and be sure to
; account for order of precedence.

; For these tables you MUST have the Operator Calc directive set at the top of your main file
; or have it checked in options or BAD THINGS WILL HAPPEN!! It basically means that calculations
; will be performed BEFORE giving back the hi/lo byte with '>' rather than the default of
; hi/lo byte THEN the calculation
                                                  
SCREEN_LINE_OFFSET_TABLE_LO        
          byte <SCREEN_MEM                      
          byte <SCREEN_MEM + 40                 
          byte <SCREEN_MEM + 80
          byte <SCREEN_MEM + 120
          byte <SCREEN_MEM + 160
          byte <SCREEN_MEM + 200
          byte <SCREEN_MEM + 240
          byte <SCREEN_MEM + 280
          byte <SCREEN_MEM + 320
          byte <SCREEN_MEM + 360
          byte <SCREEN_MEM + 400
          byte <SCREEN_MEM + 440
          byte <SCREEN_MEM + 480
          byte <SCREEN_MEM + 520
          byte <SCREEN_MEM + 560
          byte <SCREEN_MEM + 600
          byte <SCREEN_MEM + 640
          byte <SCREEN_MEM + 680
          byte <SCREEN_MEM + 720
          byte <SCREEN_MEM + 760
          byte <SCREEN_MEM + 800
          byte <SCREEN_MEM + 840
          byte <SCREEN_MEM + 880
          byte <SCREEN_MEM + 920
          byte <SCREEN_MEM + 960

SCREEN_LINE_OFFSET_TABLE_HI
          byte >SCREEN_MEM
          byte >SCREEN_MEM + 40
          byte >SCREEN_MEM + 80
          byte >SCREEN_MEM + 120
          byte >SCREEN_MEM + 160
          byte >SCREEN_MEM + 200
          byte >SCREEN_MEM + 240
          byte >SCREEN_MEM + 280
          byte >SCREEN_MEM + 320
          byte >SCREEN_MEM + 360
          byte >SCREEN_MEM + 400
          byte >SCREEN_MEM + 440
          byte >SCREEN_MEM + 480
          byte >SCREEN_MEM + 520
          byte >SCREEN_MEM + 560
          byte >SCREEN_MEM + 600
          byte >SCREEN_MEM + 640
          byte >SCREEN_MEM + 680
          byte >SCREEN_MEM + 720
          byte >SCREEN_MEM + 760
          byte >SCREEN_MEM + 800
          byte >SCREEN_MEM + 840
          byte >SCREEN_MEM + 880
          byte >SCREEN_MEM + 920
          byte >SCREEN_MEM + 960

