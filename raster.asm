;===================================================================================================
;                                                                               RASTER INTERRUPTS
;===================================================================================================
;                                                                              Peter 'Sig' Hewett
;                                                                                         - 2016
; A chain of raster irq's and routines for installing/removing them
;---------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------
;                                                                               INSTALL RASTER IRQ
;---------------------------------------------------------------------------------------------------

InitRasterIRQ
        sei                     ; stop all interrupts
        lda PROC_PORT
        
        lda #$7f                ; disable cia #1 generating timer irqs
        sta INT_CONTROL         ; which are used by the system to flash cursor, etc.

        lda #$01                ; tell the VIC we want to generate raster irqs
                                ; Note - by directly writing #$01 and not setting bits
                                ; we are also turning off sprite/sprite sprite/background
                                ; and light pen interrupts.. But those are rather shite anyways
                                ; and won't be missed

        sta VIC_INTERRUPT_CONTROL

        lda #$10                ; number of the rasterline we want the IRQ to occur at
        sta VIC_RASTER_LINE     ; we used this for WaitFrame, remember? Reading gives the current
                                ; raster line, writing sets the line for a raster interrupt to occur

                                ; The raster counter goes from 0-312, so we need to set the
                                ; most significant bit (MSB)

                                ; The MSB for setting the raster line is bit 7 of $D011 - this
                                ; is also VIC_SCREEN_CONTROL, that sets the height of the screen,
                                ; if it's turned on, text or bitmap mode, and other things.
                                ; so we could easily use this 'short' method
;        lda #$14               ; screen on and at 25 rows..
;        sta $D011
                                
                                ; But doing things properly and only setting the bits we want is a
                                ; good practice to get into.
                                ; also we now have the option of turning the screen on when we want
                                ; to - like after everything is set up

        lda VIC_SCREEN_CONTROL  ; Fetch the VIC_SCREEN_CONTROL
        and #%01111111          ; mask the surrounding bits
;        ora #%00000000          ; or in the value we want to set the MSB (Most significant bit)
                                ; in this case, it's cleared
        sta VIC_SCREEN_CONTROL
                                ; set the irq vector to point to our routine
        lda #<IrqTopScreen
        sta $0314
        lda #>IrqTopScreen
        sta $0315
                                ; Acknowlege any pending cia timer interrupts
                                ; just to be 100% safe
        lda $dc0d
        lda $dd0d

        cli                     ; turn interrupts back on
        rts


;---------------------------------------------------------------------------------------------------
;                                                                              RELEASE RASTER IRQs
;---------------------------------------------------------------------------------------------------
ReleaseRasterIRQ
        sei                             ; stop all interrupts

        lda #$37                        ; make sure the IO regs at $dxxx
        sta $01                         ; are visible
        
        lda #$ff                        ; enable cia #1 generating timing irq's
        sta $dc0d

        lda #$00                        ; no more raster IRQ's
        sta $d01a

        lda #$31
        sta $0314
        lda #$ea
        sta $0315

        lda $dc0d                       ; acknowlege any pending cia timer interrupts
        lda $dd0d

        cli
        rts

;===================================================================================================
;                                                                               IRQ - TOP SCREEN 
;===================================================================================================
; Irq set to the very top of the visible screen (IN BORDER) good for screen setup and timers
;
;---------------------------------------------------------------------------------------------------
IrqTopScreen
                                ; acknowledge VIC irq
        lda $D019
        sta $D019
        
                                ; install scroller irq
        lda #<IrqScoreBoard
        sta $0314
        lda #>IrqScoreBoard
        sta $0315
        
                                 ; nr of rasterline we want the NEXT irq to occur at
        lda #$CA
        sta $D012
        ;======================================================================= OUR CODE GOES HERE
        
 ;       lda #COLOR_GREY3
 ;       sta VIC_BACKGROUND_COLOR


;       lda CURRENT_SCREEN + 1
;       cmp #$44
;       beq @backscreen

;       lda #%00000010
;       sta VIC_MEMORY_CONTROL
;       jmp @cont

;@backscreen
;       lda #%00010010
;       sta VIC_MEMORY_CONTROL

;@cont
;       lda VIC_SCREEN_CONTROL_X        ;take the current values (from scoreboard)
;       and #%11110000                  ; mask out the scroll value and bit 3
;       ora SCROLL_COUNT                ;or in the scroll value for bits 0-2
;       sta VIC_SCREEN_CONTROL_X          ;save it in the register

;----------------------------------------------------------------------------
;       jsr UpdateTimers                ;update our timers and automatic system
;       jsr ReadJoyStick        

        lda #COLOR_BLUE
        sta VIC_BORDER_COLOR

        jsr UpdateTImers                ;update our timers and automatic system
        jsr ReadJoystick
        jsr JoyButton
        
        lda #COLOR_BLACK
        sta VIC_BORDER_COLOR
      
        ;-------------------------------------------------------------------------------------------
        jmp $ea31

;===================================================================================================
;                                                                 IRQ - BOTTOM SCREEN / SCOREBOARD
;===================================================================================================    
; IRQ at the top of the scoreboard

IrqScoreBoard
                                ; acknowledge VIC irq
        lda $D019
        sta $D019
                                ; install scroller irq
        lda #<IrqTopScreen
        sta $0314
        lda #>IrqTopScreen
        sta $0315
        
                                 ; nr of rasterline we want the NEXT irq to occur at
        lda #$09
        sta $D012
        ;======================================================================= OUR CODE GOES HERE
        lda #COLOR_GREEN
        sta VIC_BORDER_COLOR

;       lda #%00001000
;       sta VIC_SCREEN_CONTROL_X ; default settings, no scroll, no multi, 40 cols

;       lda #%001100100         ; Character set 2, screen 6
;       sta VIC_MEMORY_CONTROL
        ;-------------------------------------------------------------------------------------------
        jmp $ea31        