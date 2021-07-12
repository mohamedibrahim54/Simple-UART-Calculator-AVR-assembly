;
; UART example.asm
; Created: 13/11/2018 10:26:29 م
; Author : mohamed Ibrahim
; MCU = AVR Atmega8A
; Fosc = 4 MHZ
;the three stages of program,
;stage 1 >>> the program prompt user to enter num1 and wait for user input
;stage 2 >>> the program prompt user to enter num2 and wait for user input
;stage 3 >>> the program show the result, prompt user to press any key to contiue and wait for user input
;to do software reset using watchdog reset and reset MCU to start agaian.
;the program use it's own methods to convert from ascii to integer And vice versa.
;there is check to make sure the user only enter number character.


.macro mul16		;uses: to multiply 16-bits number with 8-bits number where @0:@1 are make 16-bits number and @2 is 8-bits number.
	push r21
	mov r21,@1		;use r21 as temporary copy of 16-bits number high bits.
	mul @0,@2		;first: multiply number low bits with number@2 the multiply result will store by mul instruction in r0:r1 registers
	mov @0,r0		;and copy multiply result in our registers to make r0:r1 available for next multiply operation.
	mov @1,r1		
	mul r21,@2		;second: multiply number high bits stored in r21 with number@2.
	add @1,r0		;add multiply result low bits to number high bits because our limit 16 bits we ignore multiply result high bits.
	pop r21
.endmacro

.macro addi				;make addi instruction to all registers from r0 to r31 
push r16
mov r16, @0
subi r16, -(@1)
mov @0, r16
pop r16
.endmacro
 
.macro addi16
subi @0, low(-@2)
sbci @1, high(-@2)
.endmacro

.def status=r20				;use r20 to store the status of program, if status = 1 >>> the program prompt user to enter num1 and wait for user input
							;if status = 2 >>> the program prompt user to enter num2 and wait for user input
							;if status = 3 >>> the program show the result, prompt user to press any key to contiue and wait for user input
							;to do soft reset using watchdog reset and reset MCU to start agaian. 

.dseg
number: .byte 8				;use this memory byte to store input two ascii numbers and and output ascii result number

.cseg
.org 0x000
restart_ISR: rjmp start
.org 0x00B
	rjmp RXcomplete
	reti
	rjmp TXcomplete

.org 0x013
start:
    ldi r16,low(ramend)			;set stack pointer to ram end(ram highest address)
	out spl,r16
	ldi r16,high(ramend)
	out sph,r16

	rcall WDT_off				;disable watchdog timer because On newer AVRs, once the watchdog is enabled, then it stays enabled, even after a reset!

	rcall number_store_init	    ; set xl:xh to point to number address

	cli
	
	ldi r16,25					;set Baud Rate to 9600 when Fosc = 4 MHZ
	out UBRRL,r16
	ldi r16,0x00
	out UBRRH,r16
	ldi r16,0b10001110			; Asynchronous Operation,no Parity, 2 Stop Bits, 8-bits Character Size  
	out UCSRC,r16
	ldi r16,0b11111000			; enable RXCIE, TXCIE, UDRIE, RXEN, TXEN
	out UCSRB,r16
	ldi r18, low(2 * hi_message)		;use r18, 19 as arguments for sendmessage function 
	ldi r19, high(2 * hi_message)		;and load them (r18,r19) with message address that will be sent.
	rcall sendMessage					;send hi message
	ldi r18, low(2 * enterNum1)
	ldi r19, high(2 * enterNum1)		;send enter number1: message
	rcall sendMessage
	ldi status, 1							
	
	
	

	sei
	loop:
		rjmp loop

WDT_off:
	; reset WDT
	WDR
	; Write logical one to WDCE and WDE
	in r16, WDTCR
	ori r16, (1<<WDCE)|(1<<WDE)
	out WDTCR, r16
	; Turn off WDT
	ldi r16, (0<<WDE)
	out WDTCR, r16
ret
 


number_store_init:
	ldi xl, low(number)     ;load x with number ram memory address
	ldi xh, high(number)
	ret
sendMessage:
	mov zl,r18				;set z register to point to the first byte address of message 
	mov zh,r19
	push r17				
	
	sendChar:
		sbis UCSRA,UDRE		;wait until UDR empty falg is set to write next char , sbis(skip branch if set) will skip below instraction if UDRE is set.
		rjmp sendChar
		lpm r17,z+			;load byte to r17 , Z: Post incremented
		cpi r17, 0			;compare char with null terminated char that ends string
		breq finishSend	
		out UDR, r17		;write char to UDR(usart data register)
		rjmp sendChar


	finishSend:
		pop r17
		ret

sendNum:					;send result number string that store in ram memory
    push r16
sendNum_loop:
	sbis UCSRA,UDRE			;wait until UDR empty falg is set to write next char , sbis(skip branch if set) will skip below instraction if UDRE is set.
	rjmp sendNum_loop
	ld r16, x+
	cpi r16, 0
	breq sendNum_finish
	out UDR, r16
	rjmp sendNum_loop
sendNum_finish:
	pop r16
	ret

int_to_ascii:				;uses: convert from int to ascii and store the output result in ram memory to view it.
	push r16
	push r17
	push r18				;use r18, r19 to hold the value(100) to do compare and compare with carry 
	push r19
	clr r6					;use r6 to hold the count of 10000th 
	clr r7					;use r7 to hold the count of 1000th
	clr r8					;use r8 to hold the count of 100th
	clr r9					;use r9 to hold the count of 10th
	clr r10					;use r10 to hold the count of 1th

	count_10000:			;the steps of convert is :
	cpi r17, high(10000)	;compare the number with 10000, if equal or greater continue and increase count
	brlo count_1000			;if lower than number move to count of 1000th
	inc r6
	subi r16, low(10000)	;subtract the number with 10000 and restart the operation again , those steps are use in those count(1000th, 100th, 10th)
	sbci r17,high(10000)
	rjmp count_10000

	count_1000:
	cpi r17, high(1000)
	brlo count_100
	inc r7
	subi r16, low(1000)
	sbci r17,high(1000)
	rjmp count_1000

	count_100:				;this is special case because the rest high byte number of subtract operations can't Compare with Immediate high(100)
							;because it will give us wrong count(always =0) so we need compare low number byte with low(100)
							;and compare high byte with high(100) with carry     
	ldi r18, low(100)
	ldi r19, high(100)
	cp r16, r18				;cp register, register only
	cpc r17, r19			;also cpc register, register only
	brlo count_10
	inc r8
	subi r16, low(100)
	sbci r17,high(100)
	rjmp count_100

	count_10:
	cpi r16, 10
	brlo count_1
	inc r9
	subi r16, 10
	rjmp count_10
	count_1:
	mov r10, r16			;store the rest number direct to r10
	addi r6,'0'				;convert each digit value to ascii by add the ascii value of zero to each digit value
	st x+,r6				;store ascii code to number ram memory 
	addi r7,'0'
	st x+,r7
	addi r8,'0'
	st x+,r8
	addi r9,'0'
	st x+,r9
	addi r10,'0'
	st x+,r10
	ldi r16, 0				;store null terminated char to end numbber string
	st x+, r16
	pop r19
	pop r18
	pop r17
	pop r16
ret

soft_reset:					;do software reset to MCU using watchdog timer reset 
	wdr						;watchdog timer reset instraction
	ldi r16, (1 << WDE)     ;enable watch dog timer to reset MCU and set Watchdog Timer Prescaler(0 | 0 | 0) to 16.3 ms at VCC = 5.0V
	out WDTCR, r16
soft_reset_loop:			;loop to make watch dog fire watchdog reset
	rjmp soft_reset_loop

RXcomplete:
	ldi r16, SREG
	push r16

	in r17, UDR				;read char from UDR and store it in r17
	cpi status, 3			;if(status == 3) reset MCU with software reset
	breq soft_reset
	cpi r17,13				;if(char == enter key) start to convert number and do caculations
	breq convert_number
check_number_1:
	cpi r17, '0'			;check if(char == number) with two stage check, stage 1: check(char >= '0')  true:goto stage 2 , false: do nothing
	brsh check_number_2
	rjmp continue_rxISR
check_number_2:				;stage 2: check (char >= '9'+1) true: nop , false: store char and print it
	cpi r17, '9' + 1
	brsh continue_rxISR
	st x+, r17				;store char 
	out UDR, r17
	
continue_rxISR:
	pop r16
	out SREG,r16

	reti

convert_number:		;convert from ascii to integer
	push r16
	push r17
	ldi r21,0     ;store digit number in r21
	st x, r21    ; put a null-terminated string char
	rcall number_store_init     ;reset x with number memory address
	clr r16                   ;clear r16:r16 to store multiply result
	clr r17
	ldi r22,10				;load r22 with 10 which will multiply with
	convert_loop:				;For the '512' string, this happens:
				 ;'5' is converted to 5 by subtracting ASCII '0' from '5'. Then the result (which is still zero) is multiplied by 10. The result is zero.
				 ;Now 5 is added. Result = 5
				;Now the '1' is received and converted to 1. The result (which is 5) is multiplied by 10, so we now have 50.
				;1 is added which equals 51.
				;The '2', being the last character of the string, is again converted,
				; the result is multiplied by 10 (=510) and 2 is added. The result is 512.

		LD r21, x+
		cpi r21,0						;if (char == null terminated char that end numbber string)  goto store number
		breq store_num
		mul16 r16, r17, r22
		subi r21, '0'
		add r16, r21
		ldi r21, 0
		adc r17, r21 
		rjmp convert_loop
	store_num:
		cpi status, 1					;if(status == 1) the program get the first number  else it get the second number
		brne num2
		mov r2,r16						;use r2,r3 to store number1
		mov r3,r17
		ldi r18, low(2 * enterNum2)		;show enter number2 message that prompt user to enter num2 and wait for user input 
		ldi r19, high(2 * enterNum2)
		rcall sendMessage
		rcall number_store_init
		inc status						;icrease status to indicate to stage 2 of program
		rjmp convert_finish
		num2:
		add r16,r2						;add number1 to number2, result store in r16,r17
		adc r17,r3
		
		ldi r18, low(2 * result_message)	;swow the result message
		ldi r19, high(2 * result_message)
		rcall sendMessage
		 
		rcall number_store_init
		rcall int_to_ascii					;convert result number from integer to ascii to print it 

		rcall number_store_init				;printf(result accii coded number)
		rcall sendNum

		ldi r18, low(2 * continue_message)
		ldi r19, high(2 * continue_message)
		rcall sendMessage
		inc status							;icrease status to indicate to stage 3 (end stage our of program)
	convert_finish:
		
		pop r17
		pop r16
		rjmp continue_rxISR





TXcomplete:
	ldi r16, SREG
	push r16

	

	pop r16
	out SREG,r16
	reti


	hi_message: .db 13, "welcome to AVR Addition Program ", 0
	enterNum1: .db 13, "enter number 1: ", 0
	enterNum2: .db 13, "enter number 2: ", 0
	result_message: .db 13, " The Result is: ", 0
	continue_message: .db 13, "press any key to contiue... ", 0 