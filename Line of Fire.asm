            ;BEGIN ------------------------------------------------------------------------------ 
            ;
            ; MOVEWORKS360//LINEOFFIRE (ex. snakED)
            ; (c)feb,mar,apr,may,june 2011,mar2012
            ; wertstahl@yahoo.de -.- wertstahl/G¤P
            ;
            ; This is a specially modified exclusive
            ; version for the inclusion in
            ; CBM Program Studio!
            ;
            ; C=64 Autonomous Text Pixel Engine
            ; Super Oldschool code!!
            ;
            ; Documentation Language: english
            ;
            ; Editor: CBM Program Studio (Assembler Sourcecode)


            ; WARNING!
            ; IF YOU ADD CODE, MAKE SURE TO CHANGE THE TABLE BANK FOR COORDX (see line 685)
            ; OR ELSE: CHAOS AND DESTRUCTION!


            ; NOTE: This code is for excercise and education purposes only.
            ; Only use this in your product with proper permission from the author.
            ; Please give credit. Thank you!


            ;$30 currentvec            temp
            ;$31 currenty              temp
            ;$32 koordinate lo wert    temp
            ;$33 koordinate hi wert    temp
            ;$34 pointer               RESERVED!!
            ;$35 currentx              temp
            ;$36 newx                  temp
            ;$37 newy                  temp
            ;$60                       temp
            ;$61                       temp
            ;$64                       temp
            ;$65                       temp
            
            ; WARNING! Currently, these ZPs are modified in the idle loop. its not a good
            ; idea to modify them in the IRQ now, too!



colordelay  = #$20  ;indicates how long to wait until the color-palette is switched


*=$0801 
            byte    $0c,$08,$01,$00,$9e,$34,$30,$39,$36,$00,$00,$00,$00,$00  ; 1 sys 4096       ;basic loader


*=$1000
            sei

            lda #$00
            sta $d021
            sta $d020
            jsr cls                 ; clear screen

            jsr maketab             ; create adress table
            jsr createyvectors      ; vektortabelleadressen anlegen
            lda #$00                ; deactivate
            sta colfinact           ; colorfade (will self-enable later)
            sta ivanact             ; vector-angle modification (will self-enable later)

            jsr flushmatrx          ; fill matrixes with dead pixels

            jsr createpalette       ; create palette adresses
            jsr palcop              ; copy first palette into working space

            jsr invertchar          ; invert charset
            jsr setchar             ; set charset and base screen
 
            lda #<mainloop          ; set irq branches
            sta $0314
            ldy #>mainloop
            sty $0315

            lda #$e8                ; set rasterline for irq
            sta $d012      
            lda #$7f       
            sta $dc0d      
            lda #$1b                ; show normal singlecolor screen
            sta $d011      
            lda #$01       
            sta $d01a  
            cli

;----------------------------------------------------------------------------------------------------------------------------------


idle        ldx startpos            ; screenpos x (40 positions #$0-#$28) in x
            ldy hupos               ; screenpos y (28 positions #$0-#$1c) in y
                                    ; the actual working area can be limited by tables stopx and stopy 

            lda modpos              ; vector angle (will be modified by chbehave)
            jsr push                ; push new pixel on stack
                                    ; if no free pixel is found, after 255 pixels one thats still alive will be replaced

            jsr stackscan           ; main routine: process stacked pixels

            jsr colfin              ; fade color ram colors

            jsr chbehave            ; change general start direction gradually
            jsr wobbla              ; change direction during movement
            jsr swtcol              ; switch color palette after some time

            jmp idle                ; chars are not so time critical, thats why we put them in the idle loop.


;----------------------------------------------------------------------------------------------------------------------------------
;##################################################################################################################################

                                    ; main loop / main irq

mainloop    inc $d019
            ldx $d012
stabelize   cpx $d012
            beq stabelize


            inc $d020

            ldx #$00
waste       dex 
            bne waste

            dec $d020

            lda #$e8 
            sta $d012               ; set raster position for next interrupt
            jmp $ea7e               ; and return from interrupt


startpos    byte $13                ; screen x position. currently not modified!
hupos       byte $0e                ; screen y position. currently not modified!
modpos      byte $01                ; angle of vector. currently modified by chbehave



;##################################################################################################################################
;----------------------------------------------------------------------------------------------------------------------------------


;------------------ push coordinates to stack and enable one pixel ---------------------------------

            ; x contains x
            ; y contains y
            ; a contains angle for vector $0-$fe --> $0 = 0° and 360°, $40 = 90°, $80= 180°, $c0 = 270° and so on... 

            ; IMPORTANT: $ff = no angle, dead pixel!

push        stx $35    ;currentx
            sty $31    ;currenty
            sta $30    ;currentvec          ;store angle in currentvec
                
            ldx $34    ;pointer             ;load last known pointerposition, begin scan from there
pushit      inx
            lda coordx,x
            cmp #$ff
            bne pushit            

            lda $35    ;currentx
            sta coordx,x                    ;store numeric x value
            lda $31    ;currenty
            sta coordy,x                    ;store numeric y value
            lda $30    ;currentvec
            sta matrxval,x                  ;store angle to matrix

            stx $34
            ldx $35
        
            rts

;---------------------------------------- stackscan --------------------------------------------

            ; Since 30.11.2011: stackscan and process are now connected
            ; i have refrained from trying matrix switching for faster reads and writes into the matrix
            ; because it would only improve the speed if only few pixels are on the screen
            ; many pixels on the screen would be much slower while using matrix switching


stackscan   ldx #$00                        ;stackpointer reset
            lda #$ff                        ;empty reference byte
checkstack  cmp coordx,x                    ;position empty?
            bne foundone                    ;no, process it
rechecks    inx                             ;stackpointer+
            bne checkstack                  ;not finished, loop
            rts                             ;all pixels scanned (or stack was empty)

            lda initvals
foundone    stx $34    ;pointer             ;save stackpointer
            lda matrxval,x                  ;get values for vector, x and y
            sta $30    ;currentvec
            lda coordx,x
            sta $35    ;currentx
            lda coordy,x
            sta $31    ;currenty
            
            lda #$ff                        ; destroy pixel in stack
            sta coordx,x                    ; because its in movement

            ;-------------------------- bewegung ---------------------------------------

                                            ;$32 coordinate lo value
                                            ;$33 coordinate hi value
                                            ;$60 gradienttable lo
                                            ;$61 gradienttable hi
                                            ;$64 target lo
                                            ;$65 target hi

process     ldy $31    ;currenty            ;load coordinate y
            lda wherehi,y                   ;and translate
            sta $33                         ;
            lda wherelo,y                   ;
            sta $32                         ;to start adress of screenline
                
            lda $35   ;currentx             ;load coordinate x 
            clc                             ;clear carry
            adc $32                         ;add x offset
            bcc outrans2                    ;blockwrap?
                                            ;correct adress
            inc $33                         ;now in currenthi ($33) and currentlo ($32)
outrans2    sta $32                         ;current adress of the y-line in ZP

            lda colfinact                   ;no gfx action if
            bne nopixkill                   ;colorfade is on

                                            ;destroy
            tay                             ;graphics content
            sta ($32),y                     ;at old position

            ;--------------------------------- check y slope + calculate y ------------------------

nopixkill   ldy $30    ;currentvec          ;load current angle
            ldx translatey,y                ;the advance-sequence for this angle is in which line of the table?                                          
            lda vectslo,x                   ;load the start-adress
            sta $60                         ;of the advance-sequence-line
            lda vectshi,x                   ;in the vector table
            sta $61                         ;

            ldy $35    ;currentx            ;load old X value (!) for y slope-offset to get variations
            ldx $31    ;currenty            ;load old Y value (!) in x for calculation
            lda ($60),y                     ;get slope value (0 or 1) out of 24 values via y from $40/2
                                            ;no slope?
            beq noplus                      ;then dont change y 
            ldy $30    ;currentvec          ;load angle
            lda commandsy,y                 ;load angle-specific command (plus or minus) from table
            sta selfmody                    ;and modify the code
selfmody    nop                             ;so here we get INX, DEX or NOP from the table, depending on the angle
noplus      stx $37   ;newy                 ;write target y
            lda stopy,x                     ;read stopbittable
                                            ;are we still allowed to be here?
            bne pixdeceased                 ;nope, pixel dies

            ;--------------------------------- check y slope + calculate y ------------------------

noystop     ldy $30    ;currentvec          ;load current angle
            ldx translatex,y                ;the advance-sequence for this angle is in which line of the table?
            lda vectslo,x                   ;load the start-adress
            sta $60                         ;of the advance-sequence-line
            lda vectshi,x                   ;in the vector table
            sta $61                         ;

            ldy $31    ;currenty            ;load old Y value (!) for x slope-offset to get variations
            ldx $35    ;currentx            ;load x old for calculations
            lda ($60),y                     ;get slope value (0 or 1) out of 24 values via y from $40/2
                                            ;no slope?
            beq noplus2                     ;then dont change x
            ldy $30    ;currentvec          ;load angle
            lda commandsx,y                 ;load angle-specific command (plus or minus) from table
            sta selfmodx                    ;and modify the code
selfmodx    nop                             ;so here we get INX, DEX or NOP from the table, depending on the angle
noplus2     stx $36    ;newx                ;write target x
            lda stopx,x                     ;read stopbittable
                                            ;are we still allowed to be here?
            bne pixdeceased                 ;nope, pixel dies

            ;---------------- calc new target adress  --------------------------------------

noxstop     ldx $37    ;newy                ;load new y
            lda wherelo,x                   ;translate to screen adress
            sta $64                         ;
            lda wherehi,x                   ;
            sta $65                         ;

            ldy $36    ;newx                ;load x offset

            lda $30    ;currentvec          ;load old angle

            ;---------------------------------------------------------------------------------

            ldx ivanact                     ;is the wobble on?
            beq noivan                      ;no, so dont change the direction
            clc                     
ivan        adc #$03                        ;change the direction by a value that will be modified externally
noivan      sta $30                         ;store the new angle                      
            sta ($64),y                     ;and write the pixel, containing the ANGLE, to the screen  <<-- there is the trick

            ;---------------------------------------------------------------------------------

            lda $65                         ;translate 
            clc
            adc #$d4                        ;new target adress to color ram
            sta $65
curcol      lda #$01                        ;load currently first color of the palette (is modified externally)

            sta $(64),y                     ;and write the color to the color rom

            ;------------------------ store these new values in matrix  ----------------------

noprint     ldx $34    ;pointer
            lda $36    ;newx
            sta coordx,x
            lda $37    ;newy
            sta coordy,x
            lda $30    ;currentvec
            sta matrxval,x

            ;---------------------------------------------------------------------------------

pixdeceased ldx $34    ;pointer             ; load the pointer again
            lda #$ff                        ; load the reference dead pixel again
            jmp rechecks

;############################################################################################################################
;----------------------------------------------------------------------------------------------------------------------------
;-----------------------------subroutines with rts --------------------------------------------------------------------------


            ; this routine modifies the angle of the vector that is being pushed to the stack. 
            ; later modifications make it move in a pattern thats called "crazy ivan"

chbehave    lda modpos            ;colourwheel!
ivan2       adc #$00
            sta modpos

wobbla2     dec swtdelax+1
swtdelax    lda #$01
            cmp #$ff
            bne wobext
            lda #$03
            sta swtdelax+1

            inc wobblabl2+1
wobblabl2   lda #$00
            cmp #$7f
            bne nowob2
            lda #$00
            sta wobblabl2+1

nowob2      inc wobblor2+1
wobblor2    ldx #$00
            lda wobble,x
            cmp #$ff
            bne wobblit2
            ldx #$00
            stx wobblor2+1
wobblit2    lda wobble,x
            sta ivan2+1
            lda wobble2,x
            sta ivan2           
wobext      rts


;----------------------------------- vector-wobble-routine -------------------------------------

ivanact     nop
wobbla      inc wobblabla+1
            lda #$01
            sta ivanact
wobblabla   lda #$00
            cmp #$7f
            bne nowob
            lda #$00
            sta wobblabla+1

nowob       inc wobblor+1
wobblor     ldx #$00
            lda wobble,x
            cmp #$ff
            bne wobblit
            ldx #$00
            stx wobblor+1
wobblit     lda wobble,x
            sta ivan+1
            lda wobble2,x
            sta ivan
            rts


;----------- switching of the current fading tables --------------
            
swtcol      dec swtdelay+1      ;count down delay
swtdelay    lda #$ff            ;overflow?
            cmp #$ff
            beq palcop          ;yes, -> copy new palette
            rts
                        
palcop      lda colordelay
            sta swtdelay+1

palettecnt  lda #$00            ;load palettepointer
            asl
            tax
            lda palettelohi,x   ;adress of current palette
            sta padr1+1         ;write lowbyte 
            sta padr2+1
            inx
            lda palettelohi,x   ;adress of current palette
            sta padr1+2         ;write hibyte
            sta padr2+2

            inc palettecnt+1    ;palettepointer +1
            lda maxpalettes     ;last palette?
            cmp palettecnt+1
            bne colorsort
            lda #$00            ;yup, reset.
            sta palettecnt+1

            ;--- copy gradients from palette ------------------------------

colorsort   ldx #$10
            lda #$00
flushcolors sta colmatrx,x
            dex 
            bne flushcolors            

            ldx #$01
padr1       lda palette,x
            sta curcol+1
            tay
            inx
morecs      
padr2       lda palette,x
            sta colmatrx,y 
            tay
            beq colorfll
            inx
            jmp morecs
          
colorfll    ldx #$00            ;so one can ready comfortably from $d800 +++ without 
colfllp     lda colmatrx,x      ;having to give a funk about the randomized 
            sta colmatrx1,x     ;high nybble, the palette must be repeated 16 times
            sta colmatrx2,x     ;because an AND #$0F would just be a terrible
            sta colmatrx3,x     ;waste of processor time
            sta colmatrx4,x
            sta colmatrx5,x
            sta colmatrx6,x
            sta colmatrx7,x
            sta colmatrx8,x
            sta colmatrx9,x
            sta colmatrxa,x
            sta colmatrxb,x
            sta colmatrxc,x
            sta colmatrxd,x
            sta colmatrxe,x
            sta colmatrxf,x   
            inx
            cpx #$10
            bne colfllp  
            rts       

;---------------------- fade the colors according to the table  -----------------------------------

colfinact   nop
colfin      lda #$01
            sta colfinact   ;routine confirms activity
            ldx #$00

d8read      ldy $d800,x
            lda colmatrx,y
            sta $d800,x

            ldy $d900,x
            lda colmatrx,y
            sta $d900,x

            ldy $da00,x
            lda colmatrx,y
            sta $da00,x

            inx
            bne d8read

danext      ldy $db00,x
            lda colmatrx,y
            sta $db00,x

            inx
            cpx #$e8            
            bne danext
            rts


;----------------------------------------------------------------------------------------------------------------------
;----------------------------- init routines --------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------

            ;----------- screen init

cls         ldy #$00                ; clear screen & color
clr         lda #$00
            sta $0400,y
            sta $0500,y
            sta $0600,y
            sta $06e8,y
            lda #$01                ; char color 
            sta $d800,y
            sta $d900,y
            sta $da00,y
            sta $dae8,y
            iny
            bne clr
            rts

            ;--------- generate matrix adress tables

maketab     lda #$00
            ldy #$04                ;screen base adress hi
            ldx #$00                ;screen base adress lo
            sta wherelo
            sty wherehi
            inx

makmor      clc
            adc #$28                ;+28 = next line
            sta wherelo,x
            bcc ynextl              ;no address-break, continue
            iny
ynextl      sta $40                 ;rescue accu
            tya
            sta wherehi,x
            lda $40                 ;get back accu
            inx
            cpx #$19                ;max lines reached?
            bne makmor
            rts

            ;----------- generate adress tables for pixel vector gradients

createyvectors
            ldx #$00
            lda #<vects             ;vector table lo
            sta vectslo
            ldy #>vects             ;vector table hi
            sty vectshi
            inx

makmor3     clc
            adc #$28                ;lenght of a table line is $28 bytes
            sta vectslo,x
            bcc yster3
            iny
yster3      sta $50                 ;rescue accu
            tya
            sta vectshi,x
            lda $50                 ;get back accu
            inx
            cpx #$1a                ;table has $1a lines
            bne makmor3
            rts

            ;----------- fill matrixes with dead pixels

flushmatrx  ldx #$00
            lda #$ff
fllflush    sta coordx,x
            sta coordy,y
            sta matrxval,x
            inx 
            bne fllflush
            rts

            ;-------------- init characterset

setchar     lda $d018
            and #$f1                ;#%11110001 ;kill charmem addr
            ora #$0c                ;#%00000010 ;set charmem to 3000
            sta $d018
            rts

            ;-------------- charset inverter 

invertchar  rts

            lda fontzor
            eor #$ff
invertoo    sta fontzor

            inc invertchar+1
            inc invertoo+1
            lda invertchar+1
            cmp #$00
            bne invertchar
            inc invertchar+2
            inc invertoo+2
            lda invertchar+2
            cmp #$3e
            bne invertchar
            rts

            ;------------------------ Generate palette adressses 

createpalette
            ldx #$00
            ldy #$00
cp_lo       lda #<palette           ;palette low adress
            sta palettelohi,x       ;to first position in table
            inx                     ;advance one step
cp_hi       lda #>palette           ;palette hi adress
            sta palettelohi,x       ;to second position in table
            inx                     ;advance one step
            lda cp_lo+1             ;load value adress low 
            clc
            adc #$10                ;add #$10 (length of palette line)
            bcc nocp_hi             ;overflow?
            inc cp_hi+1             ;yep, blockwrap
nocp_hi     sta cp_lo+1             ;write back value
            iny                     ;counter +1
            cpy maxpalettes         ;already 7 palettes?
            bne cp_lo               ;nope, go again!
            rts



;-------------------------------------------------------------------------------------------------------------------
;------------------------------------- tables ----------------------------------------------------------------------


           ;------------------------ palette-adresses auto generated --------------------------------------------
palettelohi byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

           ;------------------------ palette-matrixes auto generated --------------------------------------------
           ;having 16 matrixes speeds up the colorfade immensely, because we don´t need to check for the randomized
           ;high nybble of the color ram.
            ;     00  01  02  03  04  05  06  07  08  09  0a  0b  0c  0d  0e  0f
colmatrx    byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx1   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx2   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx3   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx4   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx5   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx6   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx7   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx8   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx9   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxa   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxb   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxc   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxd   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxe   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxf   byte  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 


           ;------------------------ palettes -------------------------------------------------------------------
            ; Palettes must be set as gradients. max. 16 values.  
            ; NO REPEATING OF COLORS! FIRST VALUE _MUST_ BE 0, LAST VALUE SHOULD BE 0.
palette     byte  $00,$06,$03,$01,$0f,$0c,$0b,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;chrome
            byte  $00,$0b,$04,$01,$07,$0a,$02,$09,$00,$00,$00,$00,$00,$00,$00,$00 ;magma
            byte  $00,$05,$0d,$01,$03,$0e,$06,$0b,$00,$00,$00,$00,$00,$00,$00,$00 ;cobalt
            byte  $00,$09,$08,$01,$0f,$0c,$0b,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;chrome
            byte  $00,$02,$0a,$01,$07,$0d,$03,$05,$0b,$00,$00,$00,$00,$00,$00,$00 ;greenspan
            byte  $00,$06,$0e,$03,$01,$07,$0a,$08,$02,$00,$00,$00,$00,$00,$00,$00 ;blueupredown
            byte  $00,$0b,$0c,$0f,$01,$07,$0d,$05,$00,$00,$00,$00,$00,$00,$00,$00 ;greyupgreendown
maxpalettes = #$07 ;real amount of palette lines


           ;------------------------ wobble values --------------------------------------------------------------
wobble       byte $05,$05,$05,$05,$04,$04,$04,$03,$03,$02,$01,$00,$01,$02,$03,$03,$04,$04,$04,$05,$05,$05,$05,$ff
wobble2      byte $e9,$e9,$e9,$e9,$e9,$e9,$e9,$e9,$e9,$e9,$e9,$69,$69,$69,$69,$69,$69,$69,$69,$69,$69,$69,$69,$ff

            ;screen-adress backup
screenx     byte    $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8,$e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0,$ff
screeny     byte    $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05,$05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07,$ff


            ;adresstable for linebeginnings of the pixelmatrix, auto generated
wherelo     byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff
wherehi     byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff


*=$1600    ;putting the tables on block beginings is a good idea for timing improvements

            ;xy coordinates x=0 to $27 y=0 to $19 -- will be overwritten by the routine
coordx      byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
coordy      byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

            ;matrix for recording degrees of a pixel. internally we only calculate with 256 degrees -- will be overwritten by the routine
matrxval    byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


            ; adress table for the lines in the vector gradient table ( represents degrees, so to speak )
vectslo     byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
vectshi     byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

            ;vector gradient table (0 = no slope, 1 = slope), this table is read from both directions
vects       byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00
            byte    $01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00
            byte    $01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00
            byte    $01,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00
            byte    $01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00
            byte    $01,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$01,$00,$00
            byte    $01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01
            byte    $01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00
            byte    $01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00
            byte    $01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01
            byte    $01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00
            byte    $01,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00
            byte    $01,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01
            byte    $01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01
            byte    $01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01
            byte    $01,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01
            byte    $01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$01
            byte    $01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01
            byte    $01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01
            byte    $01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$00
            byte    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01
            byte    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
            byte    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01


            ;Translation: 360 degrees are here 256 degrees  and then translated to 25 gradient lines in the vector gradient table
            ;shared and combined

translatey  byte    $00,$00,$01,$02,$03,$03,$04,$05,$06,$07,$07,$08,$09,$0a,$0b,$0c
            byte    $0d,$0e,$0e,$0f,$10,$11,$11,$12,$13,$14,$15,$15,$16,$17,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
translatex  byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18     ; x uses the same table as y but advanced by 180° 
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$17,$16,$15,$15,$14,$13,$12,$11,$11,$10,$0f,$0e,$0e,$0d
            byte    $0c,$0b,$0a,$0a,$09,$08,$07,$07,$06,$05,$04,$03,$03,$02,$01,$00
            byte    $00,$01,$01,$02,$03,$03,$04,$05,$06,$07,$07,$08,$09,$0a,$0b,$0c
            byte    $0d,$0e,$0e,$0f,$10,$11,$11,$12,$13,$14,$15,$15,$16,$17,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$17,$16,$15,$15,$14,$13,$12,$11,$11,$10,$0f,$0e,$0e,$0d
            byte    $0c,$0b,$0a,$0a,$09,$08,$07,$07,$06,$05,$04,$03,$03,$02,$01,$00
            byte    $00,$01,$01,$02,$03,$03,$04,$05,$06,$07,$07,$08,$09,$0a,$0b,$0c
            byte    $0d,$0e,$0e,$0f,$10,$11,$11,$12,$13,$14,$15,$15,$16,$17,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18


            ;Displayborder Stoptable 1 = border (death), 0 = screen (alive)
            ;both tables contain 256 values and are read linear
            ;so if for x the first stop position (border) is naturally
            ;40 = $28, then from byte 40 on, all bytes must be set to 1
            ;if not, the routine will brutally write into ram!!!

                    ;0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
stopx       byte    $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
            byte    $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
            byte    $0,$0,$0,$0,$0,$0,$0,$0,$1,$1,$1,$1,$1,$1,$1,$1 ;byte 40 ($28) is the first "1" value
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1

                    ;0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
stopy       byte    $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
            byte    $0,$0,$0,$0,$0,$0,$0,$0,$0,$1,$1,$1,$1,$1,$1,$1 ;byte 25 ($19) is the first "1" value
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1


            ; $ca = dex  $e8 = inx selfmodifying code table. this is a shared and combined table
commandsy   byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8

commandsx   byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8

            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca

            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca

            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            
initvals    byte    $ea,$77,$65,$72,$74,$73,$74,$61,$68,$6c

*=$3000     ;------------------------ charset (these lines are handdrawn! :-)  ------------------------------------------------------
fontzor     byte $00,$00,$00,$00,$00,$00,$00,$00      
            byte $00,$00,$00,$FF,$FF,$00,$00,$00      
            byte $00,$00,$00,$FE,$FF,$01,$00,$00      
            byte $00,$00,$00,$FE,$FF,$01,$00,$00      
            byte $00,$00,$00,$FC,$FF,$03,$00,$00      
            byte $00,$00,$00,$FC,$FF,$03,$00,$00      
            byte $00,$00,$00,$F8,$FF,$07,$00,$00      
            byte $00,$00,$00,$F8,$FF,$07,$00,$00      
            byte $00,$00,$00,$F0,$FF,$0F,$00,$00      
            byte $00,$00,$80,$F8,$7F,$07,$00,$00      
            byte $00,$00,$80,$FC,$3F,$03,$00,$00      
            byte $00,$00,$C0,$FC,$3F,$03,$00,$00      
            byte $00,$00,$C0,$F8,$1F,$03,$00,$00      
            byte $00,$00,$C0,$F8,$1F,$03,$00,$00      
            byte $00,$00,$C0,$FC,$3F,$03,$00,$00      
            byte $00,$00,$C0,$FC,$3F,$03,$00,$00      
            byte $00,$00,$C0,$FC,$3F,$03,$00,$00      
            byte $00,$00,$E0,$FC,$1F,$03,$00,$00      
            byte $00,$80,$E0,$7C,$1F,$03,$00,$00      
            byte $00,$80,$F0,$7E,$0F,$01,$00,$00      
            byte $00,$80,$F0,$78,$0E,$07,$01,$00      
            byte $00,$C0,$E0,$38,$1C,$07,$03,$00      
            byte $00,$C0,$F0,$38,$0C,$07,$03,$00      
            byte $00,$C0,$E0,$38,$1E,$07,$01,$00      
            byte $00,$80,$E0,$78,$1E,$07,$01,$00      
            byte $80,$C0,$70,$38,$0C,$07,$03,$00      
            byte $80,$C0,$70,$38,$0C,$06,$03,$01      
            byte $80,$C0,$60,$38,$1C,$06,$03,$01      
            byte $80,$C0,$60,$38,$1C,$06,$03,$01      
            byte $80,$C0,$60,$30,$1C,$0E,$03,$01      
            byte $80,$C0,$60,$30,$1C,$0E,$03,$01      
            byte $80,$C0,$60,$30,$18,$0E,$07,$01      
            byte $80,$C0,$60,$30,$18,$0C,$06,$03      
            byte $C0,$60,$60,$30,$18,$0C,$06,$03      
            byte $C0,$60,$30,$30,$18,$0C,$06,$03      
            byte $C0,$60,$30,$18,$18,$0C,$06,$03      
            byte $60,$60,$30,$30,$18,$0C,$06,$06      
            byte $60,$60,$30,$18,$18,$0C,$06,$06      
            byte $60,$60,$30,$30,$18,$0C,$0C,$06      
            byte $60,$60,$30,$30,$18,$0C,$0C,$06      
            byte $60,$30,$30,$18,$18,$0C,$0C,$06      
            byte $60,$60,$30,$18,$18,$0C,$0C,$06      
            byte $60,$60,$30,$30,$18,$0C,$06,$06      
            byte $60,$60,$30,$18,$18,$0C,$06,$06      
            byte $60,$30,$30,$30,$18,$0C,$0C,$06      
            byte $60,$30,$30,$30,$18,$18,$18,$0C      
            byte $60,$30,$30,$18,$18,$18,$0C,$0C      
            byte $30,$30,$30,$18,$18,$18,$0C,$0C      
            byte $30,$30,$18,$18,$18,$18,$0C,$0C      
            byte $30,$30,$18,$18,$18,$18,$0C,$0C      
            byte $30,$30,$18,$18,$18,$18,$0C,$0C      
            byte $30,$30,$30,$18,$18,$18,$18,$0C      
            byte $30,$30,$30,$18,$18,$18,$18,$0C      
            byte $30,$30,$30,$18,$18,$18,$18,$0C      
            byte $30,$30,$30,$30,$30,$18,$18,$18      
            byte $30,$30,$30,$30,$30,$18,$18,$18      
            byte $30,$30,$30,$30,$18,$18,$18,$18      
            byte $30,$30,$30,$30,$18,$18,$18,$18      
            byte $30,$30,$30,$18,$18,$18,$18,$18      
            byte $30,$30,$30,$18,$18,$18,$18,$18      
            byte $30,$30,$18,$18,$18,$18,$18,$18      
            byte $30,$30,$18,$18,$18,$18,$18,$18      
            byte $30,$18,$18,$18,$18,$18,$18,$18      
            byte $30,$18,$18,$18,$18,$18,$18,$18      
            byte $18,$18,$18,$18,$18,$18,$18,$18      
            byte $18,$18,$18,$18,$18,$18,$18,$30      
            byte $18,$18,$18,$18,$18,$18,$18,$30      
            byte $18,$18,$18,$18,$18,$18,$30,$30      
            byte $18,$18,$18,$18,$18,$18,$30,$30      
            byte $18,$18,$18,$18,$18,$18,$30,$30      
            byte $18,$18,$18,$18,$18,$30,$30,$30      
            byte $18,$18,$18,$18,$18,$30,$30,$30      
            byte $18,$18,$18,$18,$30,$30,$30,$30      
            byte $18,$18,$18,$18,$18,$30,$30,$30      
            byte $18,$18,$18,$18,$18,$30,$30,$30      
            byte $0C,$0C,$0C,$18,$18,$18,$18,$30      
            byte $0C,$0C,$0C,$18,$18,$18,$18,$30      
            byte $0C,$0C,$0C,$18,$18,$18,$18,$30      
            byte $0C,$0C,$18,$18,$18,$18,$30,$30      
            byte $0C,$0C,$18,$18,$18,$18,$30,$30      
            byte $0C,$0C,$18,$18,$18,$18,$30,$30      
            byte $06,$0C,$0C,$18,$18,$18,$30,$30      
            byte $06,$0C,$0C,$0C,$18,$18,$18,$30      
            byte $06,$0C,$0C,$0C,$18,$30,$30,$60      
            byte $06,$06,$0C,$18,$18,$30,$60,$60      
            byte $06,$06,$0C,$0C,$18,$30,$60,$60      
            byte $06,$06,$0C,$0C,$18,$30,$60,$60      
            byte $06,$06,$0C,$18,$18,$30,$30,$60      
            byte $06,$0C,$0C,$18,$18,$30,$30,$60      
            byte $06,$06,$0C,$0C,$18,$30,$30,$60      
            byte $06,$06,$0C,$0C,$18,$30,$30,$60      
            byte $06,$06,$0C,$18,$18,$30,$60,$60      
            byte $06,$06,$0C,$0C,$18,$30,$60,$60      
            byte $03,$06,$0C,$18,$18,$30,$60,$C0      
            byte $03,$06,$0C,$0C,$18,$30,$60,$C0      
            byte $03,$06,$06,$0C,$18,$30,$60,$C0      
            byte $01,$03,$06,$0C,$18,$30,$60,$C0      
            byte $01,$07,$0E,$18,$30,$60,$C0,$80      
            byte $01,$03,$0E,$1C,$30,$60,$C0,$80      
            byte $01,$03,$0E,$1C,$30,$60,$C0,$80      
            byte $01,$03,$06,$1C,$38,$60,$C0,$80      
            byte $01,$03,$06,$1C,$38,$60,$C0,$80      
            byte $00,$03,$07,$0C,$38,$70,$C0,$80      
            byte $00,$01,$07,$0E,$38,$70,$C0,$80      
            byte $00,$01,$07,$1E,$78,$E0,$80,$00      
            byte $00,$03,$07,$1C,$78,$E0,$80,$00      
            byte $00,$03,$0F,$1C,$30,$E0,$C0,$00      
            byte $00,$03,$07,$1C,$38,$E0,$C0,$00      
            byte $00,$01,$0F,$1E,$70,$E0,$80,$00      
            byte $00,$01,$0F,$7E,$F0,$80,$00,$00      
            byte $00,$01,$07,$3E,$F8,$C0,$00,$00      
            byte $00,$00,$07,$3F,$F8,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$C0,$00,$00      
            byte $00,$00,$03,$1F,$F8,$C0,$00,$00      
            byte $00,$00,$03,$1F,$F8,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$80,$00,$00      
            byte $00,$00,$01,$1F,$FE,$E0,$00,$00      
            byte $00,$00,$00,$0F,$FF,$F0,$00,$00      
            byte $00,$00,$00,$07,$FF,$F8,$00,$00      
            byte $00,$00,$00,$07,$FF,$F8,$00,$00      
            byte $00,$00,$00,$03,$FF,$FC,$00,$00      
            byte $00,$00,$00,$03,$FF,$FC,$00,$00      
            byte $00,$00,$00,$01,$FF,$FE,$00,$00      
            byte $00,$00,$00,$01,$FF,$FE,$00,$00      
            byte $00,$00,$00,$FF,$FF,$00,$00,$00      
            byte $00,$00,$00,$00,$00,$00,$00,$00      
            byte $00,$00,$00,$FF,$FF,$00,$00,$00      
            byte $00,$00,$00,$FE,$FF,$01,$00,$00      
            byte $00,$00,$00,$FE,$FF,$01,$00,$00      
            byte $00,$00,$00,$FC,$FF,$03,$00,$00      
            byte $00,$00,$00,$FC,$FF,$03,$00,$00      
            byte $00,$00,$00,$F8,$FF,$07,$00,$00      
            byte $00,$00,$00,$F8,$FF,$07,$00,$00      
            byte $00,$00,$00,$F0,$FF,$0F,$00,$00      
            byte $00,$00,$80,$F8,$7F,$07,$00,$00      
            byte $00,$00,$80,$FC,$3F,$03,$00,$00      
            byte $00,$00,$C0,$FC,$3F,$03,$00,$00      
            byte $00,$00,$C0,$F8,$1F,$03,$00,$00      
            byte $00,$00,$C0,$F8,$1F,$03,$00,$00      
            byte $00,$00,$C0,$FC,$3F,$03,$00,$00      
            byte $00,$00,$C0,$FC,$3F,$03,$00,$00      
            byte $00,$00,$C0,$FC,$3F,$03,$00,$00      
            byte $00,$00,$E0,$FC,$1F,$03,$00,$00      
            byte $00,$80,$E0,$7C,$1F,$03,$00,$00      
            byte $00,$80,$F0,$7E,$0F,$01,$00,$00      
            byte $00,$80,$F0,$78,$0E,$07,$01,$00      
            byte $00,$C0,$E0,$38,$1C,$07,$03,$00      
            byte $00,$C0,$F0,$38,$0C,$07,$03,$00      
            byte $00,$C0,$E0,$38,$1E,$07,$01,$00      
            byte $00,$80,$E0,$78,$1E,$07,$01,$00      
            byte $80,$C0,$70,$38,$0C,$07,$03,$00      
            byte $80,$C0,$70,$38,$0C,$06,$03,$01      
            byte $80,$C0,$60,$38,$1C,$06,$03,$01      
            byte $80,$C0,$60,$38,$1C,$06,$03,$01      
            byte $80,$C0,$60,$30,$1C,$0E,$03,$01      
            byte $80,$C0,$60,$30,$1C,$0E,$03,$01      
            byte $80,$C0,$60,$30,$18,$0E,$07,$01      
            byte $80,$C0,$60,$30,$18,$0C,$06,$03      
            byte $C0,$60,$60,$30,$18,$0C,$06,$03      
            byte $C0,$60,$30,$30,$18,$0C,$06,$03      
            byte $C0,$60,$30,$18,$18,$0C,$06,$03      
            byte $60,$60,$30,$30,$18,$0C,$06,$06      
            byte $60,$60,$30,$18,$18,$0C,$06,$06      
            byte $60,$60,$30,$30,$18,$0C,$0C,$06      
            byte $60,$60,$30,$30,$18,$0C,$0C,$06      
            byte $60,$30,$30,$18,$18,$0C,$0C,$06      
            byte $60,$60,$30,$18,$18,$0C,$0C,$06      
            byte $60,$60,$30,$30,$18,$0C,$06,$06      
            byte $60,$60,$30,$18,$18,$0C,$06,$06      
            byte $60,$30,$30,$30,$18,$0C,$0C,$06      
            byte $60,$30,$30,$30,$18,$18,$18,$0C      
            byte $60,$30,$30,$18,$18,$18,$0C,$0C      
            byte $30,$30,$30,$18,$18,$18,$0C,$0C      
            byte $30,$30,$18,$18,$18,$18,$0C,$0C      
            byte $30,$30,$18,$18,$18,$18,$0C,$0C      
            byte $30,$30,$18,$18,$18,$18,$0C,$0C      
            byte $30,$30,$30,$18,$18,$18,$18,$0C      
            byte $30,$30,$30,$18,$18,$18,$18,$0C      
            byte $30,$30,$30,$18,$18,$18,$18,$0C      
            byte $30,$30,$30,$30,$30,$18,$18,$18      
            byte $30,$30,$30,$30,$30,$18,$18,$18      
            byte $30,$30,$30,$30,$18,$18,$18,$18      
            byte $30,$30,$30,$30,$18,$18,$18,$18      
            byte $30,$30,$30,$18,$18,$18,$18,$18      
            byte $30,$30,$30,$18,$18,$18,$18,$18      
            byte $30,$30,$18,$18,$18,$18,$18,$18      
            byte $30,$30,$18,$18,$18,$18,$18,$18      
            byte $30,$18,$18,$18,$18,$18,$18,$18      
            byte $30,$18,$18,$18,$18,$18,$18,$18      
            byte $18,$18,$18,$18,$18,$18,$18,$18      
            byte $18,$18,$18,$18,$18,$18,$18,$30      
            byte $18,$18,$18,$18,$18,$18,$18,$30      
            byte $18,$18,$18,$18,$18,$18,$30,$30      
            byte $18,$18,$18,$18,$18,$18,$30,$30      
            byte $18,$18,$18,$18,$18,$18,$30,$30      
            byte $18,$18,$18,$18,$18,$30,$30,$30      
            byte $18,$18,$18,$18,$18,$30,$30,$30      
            byte $18,$18,$18,$18,$30,$30,$30,$30      
            byte $18,$18,$18,$18,$18,$30,$30,$30      
            byte $18,$18,$18,$18,$18,$30,$30,$30      
            byte $0C,$0C,$0C,$18,$18,$18,$18,$30      
            byte $0C,$0C,$0C,$18,$18,$18,$18,$30      
            byte $0C,$0C,$0C,$18,$18,$18,$18,$30      
            byte $0C,$0C,$18,$18,$18,$18,$30,$30      
            byte $0C,$0C,$18,$18,$18,$18,$30,$30      
            byte $0C,$0C,$18,$18,$18,$18,$30,$30      
            byte $06,$0C,$0C,$18,$18,$18,$30,$30      
            byte $06,$0C,$0C,$0C,$18,$18,$18,$30      
            byte $06,$0C,$0C,$0C,$18,$30,$30,$60      
            byte $06,$06,$0C,$18,$18,$30,$60,$60      
            byte $06,$06,$0C,$0C,$18,$30,$60,$60      
            byte $06,$06,$0C,$0C,$18,$30,$60,$60      
            byte $06,$06,$0C,$18,$18,$30,$30,$60      
            byte $06,$0C,$0C,$18,$18,$30,$30,$60      
            byte $06,$06,$0C,$0C,$18,$30,$30,$60      
            byte $06,$06,$0C,$0C,$18,$30,$30,$60      
            byte $06,$06,$0C,$18,$18,$30,$60,$60      
            byte $06,$06,$0C,$0C,$18,$30,$60,$60      
            byte $03,$06,$0C,$18,$18,$30,$60,$C0      
            byte $03,$06,$0C,$0C,$18,$30,$60,$C0      
            byte $03,$06,$06,$0C,$18,$30,$60,$C0      
            byte $01,$03,$06,$0C,$18,$30,$60,$C0      
            byte $01,$07,$0E,$18,$30,$60,$C0,$80      
            byte $01,$03,$0E,$1C,$30,$60,$C0,$80      
            byte $01,$03,$0E,$1C,$30,$60,$C0,$80      
            byte $01,$03,$06,$1C,$38,$60,$C0,$80      
            byte $01,$03,$06,$1C,$38,$60,$C0,$80      
            byte $00,$03,$07,$0C,$38,$70,$C0,$80      
            byte $00,$01,$07,$0E,$38,$70,$C0,$80      
            byte $00,$01,$07,$1E,$78,$E0,$80,$00      
            byte $00,$03,$07,$1C,$78,$E0,$80,$00      
            byte $00,$03,$0F,$1C,$30,$E0,$C0,$00      
            byte $00,$03,$07,$1C,$38,$E0,$C0,$00      
            byte $00,$01,$0F,$1E,$70,$E0,$80,$00      
            byte $00,$01,$0F,$7E,$F0,$80,$00,$00      
            byte $00,$01,$07,$3E,$F8,$C0,$00,$00      
            byte $00,$00,$07,$3F,$F8,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$C0,$00,$00      
            byte $00,$00,$03,$1F,$F8,$C0,$00,$00      
            byte $00,$00,$03,$1F,$F8,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$C0,$00,$00      
            byte $00,$00,$03,$3F,$FC,$80,$00,$00      
            byte $00,$00,$01,$1F,$FE,$E0,$00,$00      
            byte $00,$00,$00,$0F,$FF,$F0,$00,$00      
            byte $00,$00,$00,$07,$FF,$F8,$00,$00      
            byte $00,$00,$00,$07,$FF,$F8,$00,$00      
            byte $00,$00,$00,$03,$FF,$FC,$00,$00      
            byte $00,$00,$00,$03,$FF,$FC,$00,$00      
            byte $00,$00,$00,$01,$FF,$FE,$00,$00      
            byte $00,$00,$00,$01,$FF,$FE,$00,$00      
            byte $00,$00,$00,$FF,$FF,$00,$00,$00     
 
;------------------------------------------------------------------- EOF
