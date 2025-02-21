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
.def	mpr2 = r23
.def	waitcnt = r17			; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter

.equ	WTime = 100				; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit
.equ	ResetCnt = 3

.equ	EngEnR = 5				; Right Engine Enable Bit
.equ	EngEnL = 6				; Left Engine Enable Bit
.equ	EngDirR = 4				; Right Engine Direction Bit
.equ	EngDirL = 7				; Left Engine Direction Bit

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00				; Move Backward Command
.equ	TurnR = (1<<EngDirL)			; Turn Right Command
.equ	TurnL = (1<<EngDirR)			; Turn Left Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command


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
		rcall	HitRight		; Call function to handle interrupt
		reti					; Return from interrupt

.org	$0004					
		rcall	HitLeft		; Call function to handle interrupt
		reti					; Return from interrupt

.org	$0008					
		rcall	CLEAR		; Call function to handle interrupt
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

		; Initialize LCD Display
		rcall LCDInit
		rcall CLEAR ; clears garbage values

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


		; Initialize external interrupts
			; Set the Interrupt Sense Control to falling edge 
		ldi		mpr, 0b10001010
		sts		EICRA, mpr

		; Configure the External Interrupt Mask
		ldi		mpr, (1 << WskrL | 1 << WskrR | 1 << ResetCnt)
		out		EIMSK, mpr ;enable port 0 and 1 for interrupts

		; Turn on interrupts
			; NOTE: This must be the last thing to do in the INIT function
		sei							

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		ldi		mpr, MovFwd		; Load Move Forward Command
		out		PORTB, mpr		; Send command to motors

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


;----------------------------------------------------------------
; Sub:	UpdateCount
; Desc:	Updates the whisker hit counters on the LCD
;----------------------------------------------------------------
UpdateCount:
		ldi YL, low(0x0108)
		ldi YH, high(0x0108)
		st	Y, ilcnt

		ldi YL, low(0x0118)
		ldi YH, high(0x0118)
		st	Y, olcnt

		rcall LCDWrite
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	HitRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
HitRight:
		cli
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		lds  mpr, isr_flag      ; Load ISR flag from SRAM
		cpi  mpr, 1
		breq HitRightEnd

		ldi  mpr, 1             ; Set ISR running flag
		sts  isr_flag, mpr      ; Store in RAM


		ldi		mpr, (0 << WskrL | 0 << WskrR | 0 << ResetCnt)
		out		EIMSK, mpr ;enable port 0 and 1 for interrupts

		

		inc		ilcnt	; increment our right whisker hit counter
		rcall	UpdateCount

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Move Forward again
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		ldi		mpr, (1 << WskrL | 1 << WskrR | 1 << ResetCnt)
		out		EIMSK, mpr ;enable port 0 and 1 for interrupts
		sei	; enable interrupts
				

		ldi  mpr, 0             ; Clear ISR running flag
		sts  isr_flag, mpr      ; Store in RAM


		in   mpr, EIFR           ; Read EIFR
		ori  mpr, (1 << INTF0)   ; Clear INTF0
		out  EIFR, mpr           ; Write back

		HitRightEnd:
		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr

		ret				; Return from subroutine


;----------------------------------------------------------------
; Sub:	HitLeft
; Desc:	Handles functionality of the TekBot when the left whisker
;		is triggered.
;----------------------------------------------------------------
HitLeft:
		cli
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		lds  mpr, isr_flag      ; Load ISR flag from SRAM
		cpi  mpr, 1
		breq HitLeftEnd

		

		ldi  mpr, 1             ; Set ISR running flag
		sts  isr_flag, mpr      ; Store in RAM
		ldi		mpr, (0 << WskrL | 0 << WskrR | 0 << ResetCnt)
		out		EIMSK, mpr ;enable port 0 and 1 for interrupts

		

		inc		olcnt	; increment our right whisker hit counter
		rcall	UpdateCount

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Turn left for a second
		ldi		mpr, TurnR	; Load Turn Right Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Move Forward again
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		ldi		mpr, (1 << WskrL | 1 << WskrR | 1 << ResetCnt)
		out		EIMSK, mpr ;enable port 0 and 1 for interrupts
		sei	; enable interrupts

		ldi  mpr, 0             ; Clear ISR running flag
		sts  isr_flag, mpr      ; Store in RAM

		in   mpr, EIFR           ; Read EIFR
		ori  mpr, (1 << INTF1)   ; Clear INTF1
		out  EIFR, mpr           ; Write back

		HitLeftEnd:
		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr

		
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly
;		waitcnt*10ms.  Just initialize wait for the specific amount
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			(((((3*ilcnt)-1+4)*olcnt)-1+4)*waitcnt)-1+16
;----------------------------------------------------------------
Wait:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt		; Decrement wait
		brne	Loop			; Continue Wait loop

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret				; Return from subroutine

;-----------------------------------------------------------
; Func: COPY_LOOP
; Desc: Copies from program memory to the memory pointed to by Y.
;-----------------------------------------------------------
COPY_LOOP:
		lpm ; loads the byte pointed to in Z register into r0
		tst r0 ; sets the flag if the byte in r0 is equal to 0 or negative
		breq END_COPY ; checks the flag, if set, then end copy

		st Y+, r0 ; stores r0 in y, then increment y
		adiw ZL, 1 ; increments ZL pointer
		RJMP COPY_LOOP ; repeat until 0

;-----------------------------------------------------------
; Func: END_COPY
; Desc: COPY_LOOP helper function
;-----------------------------------------------------------
END_COPY:							; Begin a function with a label
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: CLEAR
; Desc: Clears the screen, then copies back over our strings
;-----------------------------------------------------------
CLEAR:
		rcall LCDClr ; clear our LCD to get rid of any stray characters
		ldi ZL, low(STRING_ONE * 2) ; load the first 8 bits of our string_one ptr into ZL (r30)
		ldi ZH, high(STRING_ONE * 2) ; load the last 8 bits of our string_one ptr into ZH (r31)

		ldi YL, low(0x0100) ; load the first 8 bits of our SRAM upper 16 character ptr into YL (r28)
		ldi YH, high(0x0100) ; load the last 8 bits of our SRAM upper 16 character ptr into YL (r29)
		rcall COPY_LOOP ; call copy subroutine

		ldi ZL, low(STRING_TWO * 2) ; load the first 8 bits of our string_two ptr into ZL (r30)
		ldi ZH, high(STRING_TWO * 2) ; load the last 8 bits of our string_two ptr into ZH (r31)

		ldi YL, low(0x0110) ; load the first 8 bits of our SRAM lower 16 character ptr into YL (r28)
		ldi YH, high(0x0110) ; load the last 8 bits of our SRAM lower 16 character ptr into YL (r29)
		rcall COPY_LOOP ; call copy subroutine
		ldi	ilcnt, 48	; 48 is zero in ASCII
		ldi olcnt, 48	; 48 is zero in ASCII
		rcall LCDWrite
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


.dseg                  ; Switch to data segment (SRAM)
.org 0x0120
isr_flag: .byte 1      ; Allocate 1 byte for the flag
.cseg                  ; Switch back to code segment
;***********************************************************
;*	Stored Program Data
;***********************************************************

STRING_ONE:
.DB	"R Hits: 0", 0 ; Declaring data in ProgMem
STRING_TWO:
.DB "L Hits: 0", 0

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver
