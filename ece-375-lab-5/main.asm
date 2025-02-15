;***********************************************************
;*	This is the skeleton file for Lab 5 of ECE 375
;*
;*	 Authors: Joseph Serra and Darren Mai
;*	   Date: 2/14/2025
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************

;many of these are from lab 1
.def	mpr = r16				; Multi-Purpose Register
.def	waitcnt = r17			; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter

.equ	WTime = 100				; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit

.equ	EngEnR = 5				; Right Engine Enable Bit
.equ	EngEnL = 6				; Left Engine Enable Bit
.equ	EngDirR = 4				; Right Engine Direction Bit
.equ	EngDirL = 7				; Left Engine Direction Bit


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used

		; This is just an example:
;.org	$002E					; Analog Comparator IV
;		rcall	HandleAC		; Call function to handle interrupt
;		reti					; Return from interrupt

.org	$0002					
		rcall	rWHISKER		; Call function to handle interrupt
		reti					; Return from interrupt

.org	$0004					
		rcall	lWHISKER		; Call function to handle interrupt
		reti					; Return from interrupt

.org	$0056					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer 
		; from lab 1
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND

	   ; Initialize Port B for output
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low

		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State

		; Initialize TekBot Forward Movement
		ldi		mpr, MovFwd		; Load Move Forward Command
		out		PORTB, mpr		; Send command to motors

		; Initialize external interrupts
			; Set the Interrupt Sense Control to falling edge 
		ldi		mpr, 0b00001010
		sts		EICRA, mpr

		; Configure the External Interrupt Mask
		ldi		mpr, (1 << WskrL | 1 << WskrR)
		out		EIMSK, mpr ;enable port 0 and 1 for interrupts

		; Turn on interrupts
			; NOTE: This must be the last thing to do in the INIT function
		sei							

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		; TODO

		rjmp	MAIN			; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
;	You will probably want several functions, one to handle the
;	left whisker interrupt, one to handle the right whisker
;	interrupt, and maybe a wait function
;------------------------------------------------------------

;-----------------------------------------------------------
; Func: rWHISKER
; Desc: When the right whisker is hit, the bot backs up and turns left
;-----------------------------------------------------------
rWHISKER:							; Begin a function with a label

		;save variable by pushing them to the stack
		push	mpr	;save mpr
		in		mpr, SREG
		push	mpr ;save the status register


		ret


;-----------------------------------------------------------
; Func: lWHISKER
; Desc: When the left whisker is hit, the bot goes back and turns right
;-----------------------------------------------------------
lWHISKER:							; Begin a function with a label

		;save variable by pushing them to the stack
		push	mpr	;save mpr
		in		mpr, SREG
		push	mpr ;save the status register


		ret

;----------------------------------------------------------------
; Sub:	Wait
; Desc:	
;----------------------------------------------------------------
Wait:
	push	waitcnt ;save registers
	push	ilcnt
	push	olcnt


		ret


;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label

		; Save variable by pushing them to the stack

		; Execute the function here

		; Restore variable by popping them from the stack in reverse order

		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
