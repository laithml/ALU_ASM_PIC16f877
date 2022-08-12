LIST 	P=PIC16F877
		include	<P16f877.inc>
 __CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_OFF & _HS_OSC & _WRT_ENABLE_ON & _LVP_OFF & _DEBUG_OFF & _CPD_OFF

		org			0x00
reset:	goto		start

		org			0x10
start:	bcf			STATUS,RP0
		bcf			STATUS,RP1		;Bank0
		
;------A 0x30
		clrf 0x70
		clrf 0x71	
		clrf 0x72
		clrf 0x73
;------B 0x40
		clrf 0x74
		clrf 0x75
		clrf 0x76
		clrf 0x77
;------C 0x50
		clrf 0x78
		clrf 0x79
		clrf 0x7A
		clrf 0x7B	
		clrf 0x60

		clrf		PORTD    ;print to screen
		clrf		PORTE    ;print lcd
		bsf			STATUS,RP0		;Bank1
		bcf			INTCON,GIE		;No interrupt
		movlw		0x06
		movwf		ADCON1
		clrf		TRISE		;porte output 
		clrf		TRISD		;portd output
		movlw		0x0F
		movwf		TRISB		;portb input
		bcf			OPTION_REG,0x7	;Enable PortB Pull-Up
		bcf			STATUS,RP0		;Bank0
		call init
;--------------------------------------------------------------------------------------------------
		



main:	clrf 0x47 	;store input from keyboard in 0x47
	 	call wkb
		call delay_500m
		call check   
		call opCheck




;------------------OPCHECK

opCheck:  movlw .1
		  subwf 0x50,w
		  clrc
		  btfsc STATUS,Z
		  goto  SUBS
		  movlw .2
		  subwf 0x50,w
		  clrc
		  btfsc STATUS,Z
		  goto  MULT
		  movlw .3
		  subwf 0x50,w
		  clrc
		  btfsc STATUS,Z
		  goto  DEV
		  movlw .4
		  subwf 0x50,w
		  clrc
		  btfsc STATUS,Z
		  goto  POW
		  movlw .5
		  subwf 0x50,w
		  clrc
		  btfsc STATUS,Z
		  goto countOneA
	      movlw .6
		  subwf 0x50,w
		  clrc
		  btfsc STATUS,Z
		  goto countZeroB
		  goto ERRORLCD

;----------------OPCODE----------------------
SUBS:	movfw 0x30  
		movwf 0x58;temp A
		movfw 0x40
		movwf 0x57 ;temp B
		clrc
		rrf 0x58
		clrc
		rrf 0x57
		clrc
		movfw 0x57	;move B to w
	   subwf  0x58,W 
		btfss  STATUS,C 
		goto ASmall  ;B>A
		goto Abig  ;B<A

ASmall ;B>A
checkA: btfss 0x30,0
		goto  checkB
		goto  checkB2
checkB:	;A Posstive
		btfss 0x40,0
		goto NEGSubA ;A+ B+ 
		goto POSSumA  ;A+ B-
checkB2:;A Negative
		btfss 0x40,0
		goto NEGSumA ;A- B+
		goto POSSubA ;A- B-

NEGSubA:;B>A  
		movfw 0x58  ;A
		subwf 0x57,W  ;B-A
		movwf 0x60
		goto negRes
POSSubA:;B>A  
		;goto ERRORLCD
		movfw 0x58  ;A
		subwf 0x57,W  ;B-A
		movwf 0x60
		goto posRes

NEGSumA:;B>A  
		movfw 0x58  ;A
		addwf 0x57,W  ;B+A
		movwf 0x60
		goto negRes
POSSumA:;B>A  
		movfw 0x58  ;A
		addwf 0x57,W  ;B+A
		movwf 0x60
		goto posRes


Abig:;A>B	
checkA1: btfss 0x30,0
		goto  checkB1
		goto  checkB3
checkB1:	;A Posstive
		btfss 0x40,0
		goto POSSubB  ;A+ B+
		goto POSSumA
checkB3:;A Negative
		btfss 0x40,0
		goto NEGSumA
		goto NEGSubB;A- B- 

POSSubB:movfw 0x57 ;B
		subwf 0x58,W;A-B
		movwf 0x60
		goto posRes

		
NEGSubB:	movfw 0x57 ;B
		subwf 0x58,W;A-B
		movwf 0x60
		goto negRes


countOneA:		clrf 0x60
				btfsc 0x30,0
				incf 0x60
				btfsc 0x30,1
				incf 0x60
				btfsc 0x30,2
				incf 0x60
				btfsc 0x30,3
				incf 0x60
				goto result


countZeroB:		clrf 0x60
				btfss 0x40,0
				incf 0x60
				btfss 0x40,1
				incf 0x60
				btfss 0x40,2
				incf 0x60
				btfss 0x40,3
				incf 0x60
				goto result

MULT:        	clrf    0x60    ;clear result registers
                movfw   0x40    ;B
				movwf   0x55	    ;Decimal B
    
MLOOP:   	   movfw  0x30   ;A
			   addwf  0x60,f
               decfsz  0x55,f
               goto    MLOOP
 
               goto result


DEV:        	clrf    0x60    ;clear result registers
                movfw   0x40    ;B
				movwf   0x55	    ;temp B
				movfw   0x30   ;A
				movwf   0x54   ;temp A
	     		movlw   .0
		  		subwf   0x40,w
		 		clrc
		  		btfsc STATUS,Z
				goto  ERRORLCD
    			
DLOOP:     	    clrc
			   movfw  0x55 ;B
			   subwf   0x54,f  ;A-B->B
			   btfss   STATUS,C
			   goto    result
				incf   0x60	;result++
               goto    DLOOP

;----------------------------------------POWER
POW:
		clrw
		movfw   0x40
		movwf	0x62
		movlw	.1
		subwf	0x62,W
		btfss	STATUS,0
		goto 	RESULT1				; B = 0
		goto	res1


res1:								; if B > 0
		movfw 0x30
		movwf	0x61
		clrc
		movlw	.1
		subwf	0x61,W
		btfss	STATUS,0
		goto	result2				; A = 0  &&  B > 0
		
		movfw   0x30
		movwf	0x61
		movlw	.1
		subwf	0x61,W
		btfsc	STATUS,Z ;A !=1
		goto 	RESULT1	 ;print 1
		goto	result4
		
		
result2:   					; print 0
		movlw	.0
		movwf	0x60
		goto	result

		
result4:				;if A > 1 B != 0
		movfw   0x40
		movwf	0x62
		movlw	.1
		subwf	0x62,W
		btfsc	STATUS,Z
		goto 	printA			; B = 1
		goto	POWMUL				
printA:
		movfw   0x30		
		movwf	0x60
		goto	result
;-------------------------------------------------

;-------------------------------------------------
POWMUL:
		clrw
		clrf	0x60
		clrf	0x64
		movfw   0x40
		movwf	0x62			;0x62 = B
		
		movfw   0x30
		movwf	0x61			;0x61 = A
		movwf	0x63			;0x63 = A
		movlw   .1

mul:
		clrc
		addwf	0x60,f 	
		decfsz	0x63,f
		goto mul
		decfsz	0x62		; B--
		goto Start
		goto result

Start	
	
	    movfw   0x30
		movwf	0x63			; 0x63 = A
		clrw
		addwf	0x60,w	; save the result in w 
		clrf	0x60            ; 0x60 = 0
		goto 	mul

;*************
;BCD counter

bcdcount:
                         clrf    0x41    ; ones
                         clrf    0x42    ; tens
                         clrf    0x43    ; hundreds
                         
Count100s:               movf 0x45,w         ; the number in w 
                         movlw 0x64              ; move 100 to w
                         subwf 0x45,F        ; Substruct 0x45 - 100 to calculate 100's in the number
                         btfss STATUS,C  ; Check C ;if C=0  there is a borrow and the result is negative 
                         goto Find10s    
                         incf 0x43,F     ; incf 0x43,F
                         goto Count100s

Find10s:                movlw 0x64              ; Put 100 into W
                         addwf 0x45,F        ; Fix last 100's substraction by adding 100 to bintobcd (Currently 0x30 is negative)
                      
Count10s:       
                         movlw 0x0A             ; Put 10 into W 
                         subwf 0x45,F        ; Substruct 0x45 - 10 to calculate 10's in the number
                         btfss STATUS,C          ; Check C ;if C=0  there is a borrow and the result is negative 
                         goto Find1s
                         incf 0x42,F     ; Puts 10's  in 0x42
                         goto Count10s
						 
Find1s:
                         movlw 0x0A            ; Put 10 into W
                         addwf 0x45, f        ; Fix last 10's substraction by adding 10 to bintobcd (Currently 0x30 is negative)
                         movf 0x45,W         ; Now 0x45 contains the 1's 
                         movwf 0x41               ;Puts 1's  in 0x41
					                  
                                             
                        return  












;------------------------------------------------------------------------
Areg: 
		clrf 0x30 ; clear the a reg
		rlf  0x73,f   ;moving the thousands to the left 1 time and save in f
		rlf  0x73,f   ;moving the thousands to the left 1 time and save in f
		rlf  0x73,w	  ;moving the thousands to the left 1 time and save in w
		movwf 0x30     ;add w to 0x30
		rlf  0x72,f    ;hundreds
		rlf  0x72,w
		addwf 0x30,f
		rlf  0x71,w    ;tens
		addwf  0x30,f
		movlw .0
		addwf 0x70,w   ;ones
		addwf 0x30,f
		return
Breg:     ;like Areg
		clrf 0x40
		rlf  0x77,f
		rlf  0x77,f
		rlf  0x77,w	
		movwf 0x40
		rlf  0x76,f
		rlf  0x76,w
		addwf 0x40,f
		rlf  0x75,w
		addwf  0x40,f
		movlw .0
		addwf 0x74,w
		addwf 0x40,f
		return
			
Creg:      ;like Areg
		clrf 0x50
		rlf  0x7B,f
		rlf  0x7B,f
		rlf  0x7B,w	
		movwf 0x50
		rlf  0x7A,f
		rlf  0x7A,w
		addwf 0x50,f
		rlf  0x79,w
		addwf  0x50,f
		movlw .0
		addwf 0x78,w
		addwf 0x50,f
		return




;-------------------------------------------------------------------------
check:  movlw  0x0A
		subwf  0x47,W
		btfsc  STATUS,Z
		goto inputsNumA
		movlw  0x0B
		subwf  0x47,W
		btfsc  STATUS,Z
		goto inputsNumB	
		movlw  0x0C
		subwf  0x47,W
		btfsc  STATUS,Z
		goto inputsNumC
		return


inputsNumA:    
		clrf 0x73
		clrf 0x72
		clrf 0x71
		clrf 0x70
		     
		call 		displayA   ; Display A on the screen.
		clrf		0x47
		call		wkb1
		movfw		0x47     ;display first number after A
		movwf		0x73
		call		displayA
		call 		delay_500m

		clrf		0x47      ;display 2nd number after A
		call		wkb1
		movfw		0x47
		movwf		0x72
		call		displayA
		call		delay_500m
		
		clrf		0x47       ;display 3rd number after A
		call		wkb1
		movfw		0x47
		movwf		0x71
		call		displayA
		call		delay_500m

		clrf		0x47        ;display 4th number after A
		call		wkb1
		movfw		0x47
		movwf		0x70
		call		displayA
		call		delay_500m
		
		call Areg
		goto main



displayA:	
				
		movlw		0x80		 ;PLACE for the data on the LCD
		movwf		0x20
		call 		lcdc

		movlw		0x41 		 ;A
		movwf		0x20		
		call 		lcdd

		movlw		0x30
		addwf		0x73,w
		movwf		0x20		
		call 		lcdd
		
		
		clrw
		movlw		0x30
		addwf		0x72,w
		movwf		0x20
		call 		lcdd
		
			
		
		
		movlw		0x30
		addwf		0x71,w
		movwf		0x20		
		call 		lcdd
		

		movlw		0x30
		addwf		0x70,w
		movwf		0x20		
		call 		lcdd
		
				
		
		return	

inputsNumB:
		clrf 0x77
		clrf 0x76
		clrf 0x75
		clrf 0x74
		call		displayB
		clrf		0x47
		call		wkb1
		movfw		0x47
		movwf		0x77
		call		displayB
		call 		delay_500m

		clrf		0x47
		call		wkb1
		movfw		0x47
		movwf		0x76
		call		displayB
		call		delay_500m
		
		clrf		0x47
		call		wkb1
		movfw		0x47
		movwf		0x75
		call		displayB
		call		delay_500m

		clrf		0x47
		call		wkb1
		movfw		0x47
		movwf		0x74
		call		displayB
		call		delay_500m
		
		call Breg
		goto main



displayB:	
				
		movlw		0x85		 ;PLACE for the data on the LCD
		movwf		0x20
		call 		lcdc

		movlw		0x42		;B
		movwf		0x20		
		call 		lcdd



		movlw		0x30
		addwf		0x77,w
		movwf		0x20		
		call 		lcdd
		
		
		clrw
		movlw		0x30
		addwf		0x76,w
		movwf		0x20
		call 		lcdd
		
			
		
		
		movlw		0x30
		addwf		0x75,w
		movwf		0x20		
		call 		lcdd
		

		movlw		0x30
		addwf		0x74,w
		movwf		0x20		
		call 		lcdd
		
				
		
		return	


inputsNumC:
		clrf 0x7B
		clrf 0x7A
		clrf 0x79
		clrf 0x78
		call		displayC
		clrf		0x47
		call		wkb1
		movfw		0x47
		movwf		0x7B
		call		displayC
		call 		delay_500m

		clrf		0x47
		call		wkb1
		movfw		0x47
		movwf		0x7A
		call		displayC
		call		delay_500m
		
		clrf		0x47
		call		wkb1
		movfw		0x47
		movwf		0x79
		call		displayC
		call		delay_500m

		clrf		0x47
		call		wkb1
		movfw		0x47
		movwf		0x78
		call		displayC
		call		delay_500m
		call Creg
		goto main



displayC:	
				
		movlw		0x8A		 ;PLACE for the data on the LCD
		movwf		0x20
		call 		lcdc

		movlw		0x43		;C
		movwf		0x20		
		call 		lcdd

		movlw		0x30
		addwf		0x7B,w
		movwf		0x20		
		call 		lcdd
		
		
		clrw
		movlw		0x30
		addwf		0x7A,w
		movwf		0x20
		call 		lcdd
		
			
		
		
		movlw		0x30
		addwf		0x79,w
		movwf		0x20		
		call 		lcdd
		

		movlw		0x30
		addwf		0x78,w
		movwf		0x20		
		call 		lcdd
		
				
		
		return	
;---------------------------------------------------------------------
result:  

		movlw		0xC4		 ;PLACE for the data on the LCD
		movwf		0x20
		call 		lcdc

		movlw		0x52  ;R
		movwf		0x20		
		call 		lcdd

		movlw		0x45  ;E
		movwf		0x20		
		call 		lcdd
	
		movlw		0x53  ;S
		movwf		0x20		
		call 		lcdd
		
		movlw		0x55  ;U
		movwf		0x20		
		call 		lcdd

		movlw		0x4C  ;L
		movwf		0x20		
		call 		lcdd

		movlw		0x54  ;T
		movwf		0x20		
		call 		lcdd

		clrf 		0x61
		clrf 		0x62
		clrf 		0x63
		movfw       0x60
		movwf       0x45
		call    	bcdcount
				
		movlw		0x30
		addwf		0x43,w
		movwf		0x20		
		call 		lcdd
		
		movlw		0x30
		addwf		0x42,w
		movwf		0x20		
		call 		lcdd

		movlw		0x30
		addwf		0x41,w
		movwf		0x20		
		call 		lcdd

		goto main	

RESULT1:  

		movlw		0xC4		 ;PLACE for the data on the LCD
		movwf		0x20
		call 		lcdc

		movlw		0x52  ;R
		movwf		0x20		
		call 		lcdd

		movlw		0x45  ;E
		movwf		0x20		
		call 		lcdd
	
		movlw		0x53  ;S
		movwf		0x20		
		call 		lcdd
		
		movlw		0x55  ;U
		movwf		0x20		
		call 		lcdd

		movlw		0x4C  ;L
		movwf		0x20		
		call 		lcdd

		movlw		0x54  ;T
		movwf		0x20		
		call 		lcdd
		movlw  .1
		movwf 0x60
		clrf 		0x61
		clrf 		0x62
		clrf 		0x63
		movfw       0x60
		movwf       0x45
		call    	bcdcount
				
		movlw		0x30
		addwf		0x43,w
		movwf		0x20		
		call 		lcdd
		
		movlw		0x30
		addwf		0x42,w
		movwf		0x20		
		call 		lcdd

		movlw		0x30
		addwf		0x41,w
		movwf		0x20		
		call 		lcdd

		goto main	



	
		  





posRes:  

		movlw		0xC4		 ;PLACE for the data on the LCD
		movwf		0x20
		call 		lcdc

		movlw		0x52  ;R
		movwf		0x20		
		call 		lcdd

		movlw		0x45  ;E
		movwf		0x20		
		call 		lcdd
	
		movlw		0x53  ;S
		movwf		0x20		
		call 		lcdd
		
		movlw		0x55  ;U
		movwf		0x20		
		call 		lcdd

		movlw		0x4C  ;L
		movwf		0x20		
		call 		lcdd

		movlw		0x54  ;T
		movwf		0x20		
		call 		lcdd

		clrf 		0x61
		clrf 		0x62
		clrf 		0x63
		movfw       0x60
		movwf       0x45
		call    	bcdcount
				
		movlw       0x2B  ; "+"
		movwf		0x20		
		call 		lcdd
		
		movlw		0x30
		addwf		0x42,w
		movwf		0x20		
		call 		lcdd

		movlw		0x30
		addwf		0x41,w
		movwf		0x20		
		call 		lcdd

		goto main

negRes:  

		movlw		0xC4		 ;PLACE for the data on the LCD
		movwf		0x20
		call 		lcdc

		movlw		0x52  ;R
		movwf		0x20		
		call 		lcdd

		movlw		0x45  ;E
		movwf		0x20		
		call 		lcdd
	
		movlw		0x53  ;S
		movwf		0x20		
		call 		lcdd
		
		movlw		0x55  ;U
		movwf		0x20		
		call 		lcdd

		movlw		0x4C  ;L
		movwf		0x20		
		call 		lcdd

		movlw		0x54  ;T
		movwf		0x20		
		call 		lcdd

		clrf 		0x61
		clrf 		0x62
		clrf 		0x63
		movfw       0x60
		movwf       0x45
		call    	bcdcount
				
		movlw       0x2D  ; "-"
		movwf		0x20		
		call 		lcdd
		
		movlw		0x30
		addwf		0x42,w
		movwf		0x20		
		call 		lcdd

		movlw		0x30
		addwf		0x41,w
		movwf		0x20		
		call 		lcdd

		goto main	
;----------------------------------------------------------------------

delay_500m:					;-----> 500ms delay
		movlw		0x32			;N1 = 50d
		movwf		0x51
CONT5:	movlw		0x80			;N2 = 128d
		movwf		0x52
CONT6:	movlw		0x80			;N3 = 128d
		movwf		0x53
CONT7:	decfsz		0x53, f
		goto		CONT7
		decfsz		0x52, f
		goto		CONT6
		decfsz		0x51, f
		goto		CONT5
		return						; D = (5+4N1+4N1N2+3N1N2N3)*200nsec = (5+4*50+4*50*128+3*50*128*128)*200ns = 496.7ms=~500ms

wkb1:	bcf			PORTB,0x4		;scan Row 1
		bsf			PORTB,0x5
		bsf			PORTB,0x6
		bsf			PORTB,0x7
		btfss		PORTB,0x0
		goto		kb01
		btfss		PORTB,0x1
		goto		kb02
		btfss		PORTB,0x2
		goto		kb03
		btfss		PORTB,0x3
		goto		kb0a1

		bsf			PORTB,0x4
		bcf			PORTB,0x5		;scan Row 2
		btfss		PORTB,0x0
		goto		kb04
		btfss		PORTB,0x1
		goto		kb05
		btfss		PORTB,0x2
		goto		kb06
		btfss		PORTB,0x3
		goto		kb0b1

		bsf			PORTB,0x5
		bcf			PORTB,0x6		;scan Row 3
		btfss		PORTB,0x0
		goto		kb07
		btfss		PORTB,0x1
		goto		kb08
		btfss		PORTB,0x2
		goto		kb09
		btfss		PORTB,0x3
		goto		kb0c1

		bsf			PORTB,0x6
		bcf			PORTB,0x7		;scan Row 4
		btfss		PORTB,0x0
		goto		kb0e
		btfss		PORTB,0x1
		goto		kb00
		btfss		PORTB,0x2
		goto		kb0f
		btfss		PORTB,0x3
		goto		kb0d

		goto		wkb1

kb0a1:	goto        ERRORLCD
kb0b1:	goto        ERRORLCD
kb0c1:	goto        ERRORLCD



wkb:	bcf			PORTB,0x4		;scan Row 1
		bsf			PORTB,0x5
		bsf			PORTB,0x6
		bsf			PORTB,0x7
		btfss		PORTB,0x0
		goto		kb01
		btfss		PORTB,0x1
		goto		kb02
		btfss		PORTB,0x2
		goto		kb03
		btfss		PORTB,0x3
		goto		kb0a

		bsf			PORTB,0x4
		bcf			PORTB,0x5		;scan Row 2
		btfss		PORTB,0x0
		goto		kb04
		btfss		PORTB,0x1
		goto		kb05
		btfss		PORTB,0x2
		goto		kb06
		btfss		PORTB,0x3
		goto		kb0b

		bsf			PORTB,0x5
		bcf			PORTB,0x6		;scan Row 3
		btfss		PORTB,0x0
		goto		kb07
		btfss		PORTB,0x1
		goto		kb08
		btfss		PORTB,0x2
		goto		kb09
		btfss		PORTB,0x3
		goto		kb0c

		bsf			PORTB,0x6
		bcf			PORTB,0x7		;scan Row 4
		btfss		PORTB,0x0
		goto		kb0e
		btfss		PORTB,0x1
		goto		kb00
		btfss		PORTB,0x2
		goto		kb0f
		btfss		PORTB,0x3
		goto		kb0d

		goto		wkb

kb0a:	movlw		0x0A
		goto		disp
kb0b:	movlw		0x0B
		goto		disp
kb0c:	movlw		0x0c
		goto		disp
kb00:	movlw		0x00
		goto		disp
kb01:	movlw		0x01
		goto		disp

kb02:	goto        ERRORLCD
kb03:	goto        ERRORLCD
kb04:	goto        ERRORLCD
kb05:	goto        ERRORLCD
kb06:	goto        ERRORLCD
kb07:	goto        ERRORLCD
kb08:	goto        ERRORLCD
kb09:	goto        ERRORLCD
kb0d:	goto        ERRORLCD
kb0e:	goto        ERRORLCD
kb0f:	goto        ERRORLCD

disp:	movwf		0x47	;save the value of the keypad to 0x47
		return

ERRORLCD:
		call  init
		movlw	0x84			 ;PLACE for the data on the LCD
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x45			; CHAR (the data E )
		movwf	0x20
		call 	lcdd
		call	mdel

		movlw	0x52			; CHAR (the data R)
		movwf	0x20
		call 	lcdd
		call	mdel

		movlw	0x52			; CHAR (the data R)
		movwf	0x20
		call 	lcdd
		call	mdel

		movlw	0x4F			; CHAR (the data O)
		movwf	0x20
		call 	lcdd
		call	mdel

		movlw	0x52			; CHAR (the data R)
		movwf	0x20
		call 	lcdd
		call	mdel
		goto 	wait


;----------------------------------------------------------

wait	goto	wait


;
;subroutine to initialize LCD
;
init	movlw	0x30
		movwf	0x20
		call 	lcdc
		call	del_41

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	del_01

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x01		; display clear
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x06		; ID=1,S=0 increment,no  shift 000001 ID S
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x0c		; D=1,C=B=0 set display ,no cursor, no blinking
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x38		; dl=1 ( 8 bits interface,n=12 lines,f=05x8 dots)
		movwf	0x20
		call 	lcdc
		call	mdel
		return

;
;subroutine to write command to LCD
;

lcdc	movlw	0x00		; E=0,RS=0 
		movwf	PORTE
		movf	0x20,w
		movwf	PORTD
		movlw	0x01		; E=1,RS=0
		movwf	PORTE
        call	sdel
		movlw	0x00		; E=0,RS=0
		movwf	PORTE
		return

;
;subroutine to write data to LCD
;

lcdd	movlw		0x02		; E=0, RS=1
		movwf		PORTE
		movf		0x20,w
		movwf		PORTD
        movlw		0x03		; E=1, rs=1  
		movwf		PORTE
		call		sdel
		movlw		0x02		; E=0, rs=1  
		movwf		PORTE
		return

;----------------------------------------------------------

del_41	movlw		0xcd
		movwf		0x23
lulaa6	movlw		0x20
		movwf		0x22
lulaa7	decfsz		0x22,1
		goto		lulaa7
		decfsz		0x23,1
		goto 		lulaa6 
		return


del_01	movlw		0x20
		movwf		0x22
lulaa8	decfsz		0x22,1
		goto		lulaa8
		return


sdel	movlw		0x19		; movlw = 1 cycle
		movwf		0x23		; movwf	= 1 cycle
lulaa2	movlw		0xfa
		movwf		0x22
lulaa1	decfsz		0x22,1		; decfsz= 12 cycle
		goto		lulaa1		; goto	= 2 cycles
		decfsz		0x23,1
		goto 		lulaa2 
		return


mdel	movlw		0x0a
		movwf		0x24
lulaa5	movlw		0x19
		movwf		0x23
lulaa4	movlw		0xfa
		movwf		0x22
lulaa3	decfsz		0x22,1
		goto		lulaa3
		decfsz		0x23,1
		goto 		lulaa4 
		decfsz		0x24,1
		goto		lulaa5
		return




finish:
		end