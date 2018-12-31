;******************************************************************************
;* mneomic setup
;******************************************************************************

CR			EQU	$0D		; ASCII code of carriage return
LF			EQU	$0A		; ASCII code of line feed

; Interrupt registers
tios			EQU	$80		; timer IC/OC select register
tcnt			EQU	$84		; timer count register
tscr			EQU	$86		; timer system control register
tctl1			EQU	$88		; timer control register 1
tmsk1			EQU	$8C		; timer mask register 1
tmsk2			EQU	$8D		; timer mask register 2
tflg1			EQU	$8E		; timer flag register 1
tc0			EQU	$90		; timer output compare register 0
pactl			EQU	$A0		; pulse accumulator control register
intCR			EQU	$1E		; Interupt control register
pr2_1_0			EQU	%000000101
COF			EQU	%000000001
COI  			EQU 	%000000001	; Interuupt mask built of timer channel 0
IOSO			EQU	%000000001
TCO_VectNo		EQU	23
IRQE			EQU	%000000001
IRQNUM			EQU	25
PORTE			EQU 	$81

; Interupt vector
setuservector 		EQU	$F69A		; memory location to hold the starting address of 
						; SetUserVector()
getchar 		EQU 	$F682
putchar 		EQU 	$F684
CountDownOn		EQU 	$00		; PORT A
config_CountDownOn	EQU	$02		; Used to configure CountDownOn as input or output
seven_segment		EQU	$01		; PORT B
enable_seven		EQU	$03		; Used to configure seven segment as input or output

COPCTL			EQU	$16

;******************************************************************************
;* ROUTINE: MAIN ROUTINE
;******************************************************************************
;Main program: The main program calls a subroutine obtain a countdown value and
;passes the value to another subroutine that starts and controls the countdown. Any
;invalid values should be rejected (print an error message on the HyperTerminal). A loop
;should be implemented to continually repeat the process.

		ORG 	$800	; Beginning of SRAM

; Stack configuration
		;LDX	$4000
		;LEAS	0, X

;Program configuration
	;PORT CONFIGURATION
		; Conifguring of CountDownOn
		MOVB	#$00, config_CountDownOn	; Configures CountDownOn as a input port
		MOVB	#$FF, enable_seven		; Configures the seven segment as a output

; Interrupt configuration
; Init timer
; TCO as an Output compare
		MOVB	#$80, TSCR	; Enable Timer System
		MOVB	#IOSO, TIOS	; Enable output compare for timer channel 0
		MOVB	#COI, Tmsk1 	; Enable interrupt on Timer channel 0
		MOVB	#COF, TFLG1 	; Reset Flag on timer channel 0
		MOVB	#PR2_1_0, Tmsk2 ; Setting prescale factor to 2^5 = 32
						; Thus the TCI clock period will be 32 * 125 ns = 4 us
		; select falling edge for IRQ & enable IRQ
		BSET 	INTCR, IRQE
		;clear abort IRQ interrupt flag (global variable)
		CLR 	PORTE
		CLR 	tc0
; VECTOR SETUP
		; Loads IRQ interupt
		LDD	#ABORT
		PSHD 	
		LDAB	#IRQNUM		; IRQ interupt port
		CLRA			; Clears register A
		LDX	setuservector
		JSR	0,X	; Loads first item into interupt vector
		PULD

		; Loads timing intrupt
		LDD 	#interrupt	; Loads address of the interrupt
		PSHD 	
		LDAB	#TCO_VectNo	; Loads timer channel 0
		CLRA			; Clears register A
		LDX	setuservector
		JSR	0,X		; Loads second item into interupt vector
		PULD	

		CLI

		BCLR	TSCR, #$80	; Disables timer interrupt

;Main program							
main:	
		; prints the user message to retrieve information
		LDX 	#user_msg
		JSR	putstr
		; end of printing message

		; Begininng of user input		
		JSR 	read_countdown
		; restarts if reading of number is a failure
		CMPA	#0
		BEQ	main
; got here
		; Starts countdown sequence
		JSR 	countdown

		BRA 	main	; always restarts the program after countdown complete
		SWI
; End of main routine



;******************************************************************************
;* SUB ROUTINES: MAIN SUB ROUTINES
;******************************************************************************
;* Gets countdown information from the user
;The read countdown subroutine reads two numerical values from the HyperTerminal
;and translates them into a countdown between 01 and 99 (note that for values between 1
;and 9, you still need to press 2 keys 0 and another digit). Any invalid values (00) or other
;key strokes are rejected with an error message to the Hyperterminal. When an error
;occurs, the process of reading the two numerical keys is started over.
read_countdown

; first character input
		;Gets character input by user
		LDX getchar
		JSR 0, X
		; prints character input by user
		LDX putchar
		JSR 0, X
; check
		; checks to see if number is legit
		LDX msb
		JSR number_check
		CMPA #0
		BEQ read_end

; second character input
		;Gets character input by user
		LDX getchar
		JSR 0, X
		; prints character input by user
		LDX putchar
		JSR 0, X
; check
		; checks to see if number is legit
		LDX lsb
		JSR number_check
		CMPA #0
		BEQ read_end
						
read_end:	RTS
; End of read_countdown


;* MONITOR PORT A
;The countdown subroutine operates as a foreground job that does no processing other
;than to monitor the Port A bit-7, to which a toggle switch (called CountDownOn) is
;connected, to see if a rocket launch is to proceed. When the Port A-7 input is one
;(CountDownOn = 1), the foreground job is to allow the countdown for launching a
;rocket. The countdown is to be produced by an interrupt service routine described below.
;If the Port A-7 switch is changed to zero (CountDownOn = 0), the countdown sequence
;is to hold at its present count and then resume when the switch is changed back to one.
;After the countdown reaches 0 the rocket "Blast Off!". Indicate the blast off by flashing
;00 five times using ½ second timing. You should also write "Blast Off" on the
;hyperterminal.
countdown	

		BSET 	intCR, IRQE		; ENABLES IRQ interupt
						; Global interupt enable		

		LDD	#10000			; 		
		STD 	tc0			; Initializes tc0 to 25000 so every match creates a 100 ms delay.
		
		; Main countdown program
		; checks to see whether it should pause or commence. (Monitors port A)		
; This loop continues until msb and lsb are 0
check1_pressed:						
; 	Start of code that checks for button pushed
		; If the button is pushed, stays in one spot		
		LDAA	CountDownOn
		CMPA 	#0
		BEQ	not_pressed	
;	End of code thats checsk for button pushed			
countMsgDisplay:
		PSHX
		LDX	#cnt_dsp				
		JSR 	putstr					; Displays countdown message
		PULX
		BCLR	tscr, #$80				; DISABLES all timer interrupt

pressed:	LDAA	CountDownOn
		CMPA 	#0
		BEQ	not_pressed	
		BRA 	pressed					; Checks again if the button is pressed

not_pressed:	BSET	tscr, #$80				; ENABLES timer interrupt to commence countdown
		MOVB	#10, count				; Sets up number of oc7 operations to be performed
								; this creates a 1 second delay
inner_loop:	TST 	count		
		BNE	inner_loop				; oc0 interupt occurs here
						
; So that interupt stops occuring outside of time loop
		BCLR	tscr, #$80				; DISABLES timer interrupt to commence countdown
		
		LDX	#err_msg
		JSR	putstr
		
; end of one second delay
		JSR 	display_7segment			; Displays the numbers
		PSHA

; Number check
; Check to see if zero has been reached		
		LDAA	msb
		CMPA 	#$00
		BEQ	end_test	;		
		; Gets here if still not complete(zero reached)
		PULA		; Restores register A
		BRA	pressed	; 
; Checking of lsb register
end_test:	LDAA	lsb
		CMPA 	#$00
		BEQ	tdone	;		
		; Gets here if still not complete(zero reached)
		PULA		; Restores register A
		BRA	pressed	;
; End of the loop

		;Disabling of interupts	
tdone:		BCLR 	intCR, $01		; DISABLES IRQ interupt
		
		RTS
; End of countdown

;******************************************************************************
;* SUB LEVEL 1 SUBROUTINES
;******************************************************************************
; Passed number in reg B, checks to see if it is allowed
; Passes mem location to store value in in reg X
; Returns 1 in accumulator A if success, 0 if failure
number_check
		TBA 		; backs up B in A
		; Checks to see if number is less than #$30
		SUBA 	#$30
		CMPA	#$00
		BLT	failedCheck

		; Branches if number is greater than #$39 
		SUBA	#$09
		CMPA	#$00
		BGT	failedCheck
		
		ADDA	#$09	; Restores binary number in pproper form
		STAA 	0, X	; Stores the binary number in address provided
		LDAA	#1	; SUCCESS

		RTS
failedCheck:	
		LDAA	#0	; FAILURE
		RTS
; End of number_check


;SUBROUTINE: displays register content of msb and lsb on seven segment displays.
display_7segment
		; store registers
		PSHA
		PSHB		

		; Loads the numbers
		LDAA msb
		LDAB lsb

		; Display on seven segment
		STD 	seven_segment	; Outputs to the seven segment	
		; restore registers
		PULB
		PULA
		RTS
; End of display_7segment


;******************************************************************************
;* INTERUPTS: MAIN INTERUPTS
;******************************************************************************
;* Creates one second delay
;The interrupt routine is driven by using the 68HC12 Standard Timer Module running in
;an interrupt driven output-compare mode to generate 1 second timing intervals. The
;interrupt service routine prints out the launch countdown on the two seven segment
;display using a variable set by the countdown routine (using a received parameter).
;Countdown is to be printed on the 7-segment displays and should decrement every
;second (or as close to a second as you can get). When zero is reached, the 7 segment
;displays is to flash 00 five times using ½ second timing. (Note that the countdown is to
;start only after the user sets the CountDownOn switch to one. Once the countdown starts,
;it continues until it reaches Blast Off! unless Port A-7 is zero putting the countdown
;into a hold).
interrupt
		BSET	tflg1, #$01	; Acknowledge interupt the timer0 flag
		BCLR	tscr, #$80	; Disables timers
		LDX 	#timer_msg		; Loads blast off message
		JSR 	putstr			; prints the blast off message

		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP		
				
		
;*****************************************
		DEC 	count		; Decrements count		
;*****************************************

		
		LDAA	lsb		; Loads the lsb
		CMPA 	#$00		; checks to see if it is zero
		
		BLE	check_msb	; Branches if less than or equal
		DECA			; Decrements value by one 
		STAA	lsb		; stores value
		
check_msb:	LDAA	msb
		CMPA 	#$00		; checks to see if it is zero		
		BLE	flash		; done if the lsb and msb are zero	
		DECA			; decrements msb when lsb is zero		
		STAA	msb

		; Loads nine into lsb id msb is not zero yet
		LDAA	#$09
		STAA	lsb
		
done:		BSET	tscr, #$80	; Enables timers
		RTI

; end of interrupt

; If zero reached, this portion of code executes
flash		LDX 	#blast_msg		; Loads blast off message
		JSR 	putstr			; prints the blast off message
		CPY	#0

		LDY	#5
		;half second delay
		PSHB
		PSHX		
		LDAB	#5	;1 ECLK
		
dis:		JSR 	display_7segment			

out_loop:	LDX	#20000	;2 ECLK
		
in_loop:	PSHA
		PULA	
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		NOP
		NOP
		DBNE	X, in_loop
		DBNE	B, out_loop

		;End of half second delay		
		DBNE	Y, dis		; jumps back to dis five times
		PULX
		PULB

		
		RTI
; End of interrupt

; ABORT
;An ABORT emergency procedure has to be implemented to allow stopping the launch at
;any time. A switch connected to the IRQ microcontroller input is employed to signal
;ABORT. Write an interrupt service routine such that when the switch is pressed the
;launch sequence is aborted and the 7 segment displays is to flash 99 five times using ½
;second timing. You should also write “Aborted” on the hyperterminal. After this, the
;system control is to be returned to main program. You have to modify your previous
;main program and/or subroutines to incorporate this new ABORT facility. You must not
;return to the main program directly from the interrupt routine but rather you should get
;back to the foreground job and then return to the main program.
ABORT
		BCLR	tscr, #$80	; Disables timers
		;Prints the abort message
		LDX	#abort_msg
		JSR	putstr

		LDY	#5
		;half second delay
		LDAB	#5	; 1 ECLK
		
show:		LDX	#$99	; Loads 99 binary into X register
		STX	seven_segment; Displays on seven segment display the number 			


o_loop:		LDX	#20000	;2 ECLK
		
i_loop:		PSHA
		PULA	
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		PSHA
		PULA
		NOP
		NOP
		DBNE	X, i_loop
		DBNE	b, o_loop


		;End of half second delay		
		DBNE	Y, show		; jumps back to dis five times
		
		;LDX	$4000
		;LEAS	0, X		
		JMP 	main
; End of ABORT


; Subroutine; putstr (putstring)
; puts string with terminating \0 to the screen 
; using DBug12 putchar subroutine
; index register X should contain address of string (modified) 
putstr	ldb 1,x+	; get char
	beq leave	; all done leave
	jsr [$F684,PCr] ; ouput to screen
	bra putstr	; do next one
leave	rts

;Initializing of messages
user_msg	DB	CR,LF,"Please enter the countdown numbers: ",CR,LF,$00
abort_msg	DB	CR,LF,"Aborted.",CR,LF,$00
blast_msg	DB	CR,LF,"Blast Off!",CR,LF,$00
err_msg		DB	CR,LF,"error encountered!",CR,LF,$00
cnt_dsp		DB	CR,LF,"Countdown paused",CR,LF,$00
timer_msg	DB	CR,LF,"timer interrupt",CR,LF,$00
loop_msg	DB	"loop",$00

; Save the SP
spv		RMB	2
; Numbers entered by the user
msb		RMB	1
lsb		RMB	1	
count 		RMB 	1	

		END