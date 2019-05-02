; This program tests the LTC2308 avaliable in the newer version of the DE1-SoC board.
; Access to the input pins of the ADC is avalible at connector J15. Here is the top
; view of the connector:
;
; +--+
; |  | <-- Red power button
; +--+
;
; +-----+-----+
; + GND | IN7 |
; +-----+-----+
; + IN6 | IN5 |
; +-----+-----+
; + IN4 | IN3 |
; +-----+-----+
; + IN2 | IN1 |
; ------+-----+
; + IN0 | 5V  |
; +-----+-----+
;      J15
;
; Displays the result using the 7-segment displays and also sends it via the serial port to PUTTy.
;
; (c) Jesus Calvino-Fraga 2019
;
$NOLIST
$MODDE1SOC
$LIST

CLK EQU 33333333
BAUD EQU 115200

BRG_VAL equ (0x100-(CLK/(16*BAUD)))

TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/(12*TIMER0_RATE)))) ; The prescaler in the CV-8052 is 12 unlike the AT89LP51RC2 where is 1.
TIMER_2_RELOAD EQU (65536-(CLK/(32*BAUD)))
TIMER_0_1ms EQU (65536-(CLK/(12*1000)))

SOUND_OUT     equ P1.0

ADC_MISO bit 0xF8 ; // Read only bit
ADC_MOSI bit 0xF9 ; // Write only bit
ADC_SCLK bit 0xFA ; // Write only bit
ADC_ENN bit 0xFB ; // Write only bit

; Reset vector
org 0x0000
	ljmp MainProgram

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector (not used in this code)
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023
	reti

; Timer/Counter 2 overflow interrupt vector (not used in this code)
org 0x002B
	reti

DSEG at 30H
; variables
X: ds 4
Y: ds 4
BCD: ds 4

Temp_soak: ds 2
Time_soak: ds 1
Temp_refl: ds 2
Time_refl: ds 1
Temp_safe: ds 1

Temp_soak_display: ds 2
Time_soak_display: ds 1
Temp_refl_display: ds 2
Time_refl_display: ds 1
Temp_safe_display: ds 1


BCD_display: ds 1

sec : ds 1
pwm: ds 2
temp: ds 2
msec: ds 1
state: ds 1
updown: ds 1

runtime_sec: ds 1
runtime_min: ds 1

BSEG
MF: dbit 1
long_beep_flag: dbit 1

cseg
; These 'equ' must match the wiring between the DE1-SoC board and the LCD!
; P0 is in connector JP2.  Check "CV-8052 Soft Processor in the DE1-SoC Board: Getting
; Started Guide" for the details.
ELCD_RS equ P0.4
ELCD_RW equ P0.5
ELCD_E  equ P0.6
ELCD_D4 equ P0.0
ELCD_D5 equ P0.1
ELCD_D6 equ P0.2
ELCD_D7 equ P0.3

FT93C66_CE   EQU P2.0  ; WARNING: shared with MCP3008!
FT93C66_MOSI EQU P2.1
FT93C66_MISO EQU P2.2
FT93C66_SCLK EQU P2.3

$NOLIST
$include(LCD_4bit_DE1SoC.inc) ; A library of LCD related functions and utility macros
$include(math32.inc) ; A library of LCD related functions and utility macros
$include(FT93C66.inc)
$LIST

;                  1234567890123456    <- This helps determine the location of the counter
Temp_message:  db 'sTp:xxx  rTp:xxx', 0
Time_message:  db 'sTm: xx  rTm: xx', 0
;Time_passed:   db 'xxx',0
Off_Display:   db 'OFF     ',0
Preheat_Display:  db 'PREHEAT',0
Soak_Display:     db 'SOAK    ',0
Ramp_Display:     db 'RAMP    ',0
Reflow_Display:   db 'REFLOW   ',0
Cooldown_Display: db 'COOLDOWN',0
STOP_Display:  db 'ABORT!! ',0
Startup_message: db '\r\nEnter Password:\r\n',0

read:  db 'R',0
write: db 'W',0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
  setb ET0  ; Enable timer 0 interrupt
  setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	mov TH0, #high(TIMER0_RELOAD) ; Timer 0 doesn't have autoreload in the CV-8052
	mov TL0, #low(TIMER0_RELOAD)
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

;---------------------------------;
; Send a BCD number to PuTTY      ;
;---------------------------------;
Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	; Write most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	;lcall putchar
	; write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	;lcall putchar
	; new line
	mov a, #'\n'
	lcall putchar
	pop acc
	ret

Initialize_Serial_Port:
  ; Initialize serial port and baud rate using timer 2
	mov RCAP2H, #high(TIMER_2_RELOAD)
	mov RCAP2L, #low(TIMER_2_RELOAD)
	mov T2CON, #0x34 ; #00110100B
	mov SCON, #0x52 ; Serial port in mode 1, ren, txrdy, rxempty
	ret

putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret

getchar:
	jbc	RI,getchar_L1
	sjmp getchar
getchar_L1:
	mov	a,SBUF
	ret

SendString:
  clr a
  movc a, @a+dptr
  jz SendString_L1
  lcall putchar
  inc dptr
  sjmp SendString
SendString_L1:
	ret

Initialize_LEDs:
    ; Turn off LEDs
	mov	LEDRA,#0x00
	mov	LEDRB,#0x00
	ret

Initialize_ADC:
	; Initialize SPI pins connected to LTC2308
	clr	ADC_MOSI
	clr	ADC_SCLK
	setb ADC_ENN
	ret

LTC2308_Toggle_Pins:
  mov ADC_MOSI, c
  setb ADC_SCLK
  mov c, ADC_MISO
  clr ADC_SCLK
  ret

; Bit-bang communication with LTC2308.  Check Figure 8 in datasheet (page 18):
; https://www.analog.com/media/en/technical-documentation/data-sheets/2308fc.pdf
; The VREF for this 12-bit ADC is 4.096V
; Warning: we are reading the previously converted channel! If you want to read the
; channel 'now' call this function twice.
;
; Channel to read passed in register 'b'.  Result in R1 (bits 11 downto 8) and R0 (bits 7 downto 0).
; Notice the weird order of the channel select bits!
LTC2308_RW:
  clr a
	clr	ADC_ENN ; Enable ADC

  ; Send 'S/D', get bit 11
  setb c ; S/D=1 for single ended conversion
  lcall LTC2308_Toggle_Pins
  mov acc.3, c
  ; Send channel bit 0, get bit 10
  mov c, b.2 ; O/S odd channel select
  lcall LTC2308_Toggle_Pins
  mov acc.2, c
  ; Send channel bit 1, get bit 9
  mov c, b.0 ; S1
  lcall LTC2308_Toggle_Pins
  mov acc.1, c
  ; Send channel bit 2, get bit 8
  mov c, b.1 ; S0
  lcall LTC2308_Toggle_Pins
  mov acc.0, c
  mov R1, a


  ; Now receive the lest significant eight bits
  clr a
  ; Send 'UNI', get bit 7
  setb c ; UNI=1 for unipolar output mode
  lcall LTC2308_Toggle_Pins
  mov acc.7, c
  ; Send 'SLP', get bit 6
  clr c ; SLP=0 for NAP mode
  lcall LTC2308_Toggle_Pins
  mov acc.6, c
  ; Send '0', get bit 5
  clr c
  lcall LTC2308_Toggle_Pins
  mov acc.5, c
  ; Send '0', get bit 4
  clr c
  lcall LTC2308_Toggle_Pins
  mov acc.4, c
  ; Send '0', get bit 3
  clr c
  lcall LTC2308_Toggle_Pins
  mov acc.3, c
  ; Send '0', get bit 2
  clr c
  lcall LTC2308_Toggle_Pins
  mov acc.2, c
  ; Send '0', get bit 1
  clr c
  lcall LTC2308_Toggle_Pins
  mov acc.1, c
  ; Send '0', get bit 0
  clr c
  lcall LTC2308_Toggle_Pins
  mov acc.0, c
  mov R0, a
	setb ADC_ENN ; Disable ADC
	ret

; Converts the 16-bit hex number in [R1,R0] to a
; 5-digit packed BCD in [R4,R3,R2] using the
; double-dabble algorithm.
hex2bcd16:
	clr a
	mov R4, a ; Initialize BCD to 00-00-00
	mov R3, a
	mov R2, a
	mov R5, #16  ; Loop counter.

hex2bcd16_L1:
	; Shift binary left
	mov a, R1
	mov c, acc.7 ; This way [R1,R0] remains unchanged!
	mov a, R0
	rlc a
	mov R0, a
	mov a, R1
	rlc a
	mov R1, a

	; Perform bcd + bcd + carry using BCD arithmetic
	mov a, R2
	addc a, R2
	da a
	mov R2, a
	mov a, R3
	addc a, R3
	da a
	mov R3, a
	mov a, R4
	addc a, R4
	da a
	mov R4, a

	djnz R5, hex2bcd16_L1

	ret

; Look-up table for the 7-seg displays. (Segments are turn on with zero)
T_7seg:
  DB 40H, 79H, 24H, 30H, 19H, 12H, 02H, 78H, 00H, 10H

; Display the 4-digit bcd stored in [R3,R2] using the 7-segment displays
Display_BCD:
	mov dptr, #T_7seg
	; Display the channel in HEX5
	mov a, b
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX5, a

	; Display [R3,R2] in HEX3, HEX2, HEX1, HEX0
	mov a, R3
	swap a
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX3, a

	mov a, R3
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX2, a

	mov a, R2
	swap a
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX1, a

	mov a, R2
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX0, a

	ret

	; Send a 4-digit BCD number stored in [R3,R2] to the serial port
SendNumber:
	mov a, #'\r'
	lcall putchar
	mov a, R3
	swap a
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	;mov a, #'.'
	;lcall putchar
	mov a, R3
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	mov a, R2
	swap a
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	mov a, R2
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	ret

; Wait 1 millisecond using Timer 0
Wait1ms:
	clr	TR0
	mov	a,#0xF0
	anl	a,TMOD
	orl	a,#0x01
	mov	TMOD,a
	mov	TH0, #high(TIMER_0_1ms)
	mov	TL0, #low(TIMER_0_1ms)
	clr	TF0
	setb TR0
	jnb	TF0,$
	clr	TR0
	ret

; Wait R2 milliseconds
MyDelay:
	lcall Wait1ms
  djnz R6, MyDelay
	ret

WaitQuartSec:
	mov a, #0
	mov msec, a ; msec increments every 5.629 ms
  mov R2, #67
L9:
	mov R1, #250
L8:
	mov R0, #166
L7:
	djnz R0, L4 ; 3 cycles->3*45.21123ns*166=22.51519us
  djnz R1, L5 ; 22.51519us*250=5.629ms
  inc msec
  djnz R2, L6 ; 5.629ms*89=0.5s (approximately)
  ret

WaitHalfSec:
	mov a, #0
	mov msec, a ; msec increments every 5.629 ms
  mov R2, #133
L6:
	mov R1, #250
L5:
	mov R0, #167
L4:
	djnz R0, L4 ; 3 cycles->3*45.21123ns*166=22.51519us clock cycle
  djnz R1, L5 ; 22.51519us*250=5.629ms
  inc msec
  djnz R2, L6 ; 5.629ms*89=0.5s (approximately)
  ret

WaitPointTwoSec:
	mov a, #0
	mov msec, a ; msec increments every 5.629 ms
  mov R2, #54
LL3:
	mov R1, #250
LL2:
	mov R0, #166
LL1:
	djnz R0, LL1 ; 3 cycles->3*45.21123ns*166=22.51519us
  djnz R1, LL2 ; 22.51519us*250=5.629ms
  inc msec
  djnz R2, LL3 ; 5.629ms*36=0.2s (approximately)
  ret

Calculate_Temp:
	mov x+1, R1
	mov x, R0
	clr a
	mov x+3, a
	mov x+2, a

	load_y(24390)
	lcall mul32
	Load_y(333167)
	lcall div32
	;Load_y(20)
	;lcall add32

	mov y+1, R5
	mov y, R4
	clr a
	mov y+3, a
	mov y+2, a

	lcall add32

	mov R1, x+1
	mov R0, x
	ret

Calculate_Junction:
	mov x+1, R1
	mov x, R0
	clr a
	mov x+3, a
	mov x+2, a

	load_y(40960)
	lcall mul32
	Load_y(4095)
	lcall div32
	Load_y(27300)
	lcall sub32
	Load_y(100)
	lcall div32

	mov R5, x+1
	mov R4, x

	ret

InitialString: db '\r\nLTC2308 test program\r\n', 0

Initialize_oven:
;	mov temp_soak,#low(0x150)
;	mov Time_soak,#0x60
;	mov Temp_refl,#low(0x220)
;	mov Time_refl,#0x45
;	mov Temp_safe,#0x60

	mov a, #135
	mov Temp_soak, a
	mov a, #low(0x150)
	mov Temp_soak_display,a
	mov a, #high(0x150)
	mov Temp_soak_display+1,a

	mov a, #60
	mov Time_soak, a
	mov a, #0x60
	mov Time_soak_display, a

	mov a, #220
	mov Temp_refl, a
	mov a, #low(0x220)
	mov Temp_refl_display, a
	mov a, #high(0x220)
	mov Temp_refl_display+1, a

	mov a, #45
	mov Time_refl, a
	mov a, #0x45
	mov Time_refl_display, a

	mov a, #60
	mov Temp_safe, a
	mov a, #0x60
	mov Temp_safe_display, a

	ret

	 Save_Configuration1:
	     lcall FT93C66_Write_Enable
	     mov DPTR, #0
	     ; Save variables
	     mov a, temp_soak
	     lcall FT93C66_Write
	     inc DPTR
	     mov a, time_soak
	     lcall FT93C66_Write
	     inc DPTR
	     mov a, temp_refl
	     lcall FT93C66_Write
	     inc DPTR
	     mov a, time_refl
	     lcall FT93C66_Write
	     inc DPTR

			 mov a, temp_soak_display
			 lcall FT93C66_Write
			 inc DPTR
	     mov a, time_soak_display
	     lcall FT93C66_Write
	     inc DPTR
	     mov a, temp_refl_display
	     lcall FT93C66_Write
	     inc DPTR
	     mov a, time_refl_display
	     lcall FT93C66_Write

			 inc DPTR

	     mov a, #0x55 ; First key value
	     lcall FT93C66_Write
	     inc DPTR
	     mov a, #0xAA ; Second key value
	     lcall FT93C66_Write
	     lcall FT93C66_Write_Disable
	     ret

		Load_Configuration1:
   mov dptr, #0x0008 ;First key value location.  Must be 0x55
   lcall FT93C66_Read
   cjne a, #0x55, Load_Defaults
   inc dptr ; Second key value location.  Must be 0xaa
   lcall FT93C66_Read
   cjne a, #0xaa, Load_Defaults

				mov dptr, #0x0000
			 lcall FT93C66_Read
			 mov temp_soak, a
			 inc dptr
			 lcall FT93C66_Read
			 mov time_soak, a
			 inc dptr
			 lcall FT93C66_Read
			 mov temp_refl, a
			 inc dptr
			 lcall FT93C66_Read
			 mov time_refl, a

			 inc dptr

			 lcall FT93C66_Read
			 mov temp_soak_display, a
			 inc dptr
			 lcall FT93C66_Read
			 mov time_soak_display, a
			 inc dptr
			 lcall FT93C66_Read
			 mov temp_refl_display, a
			 inc dptr
			 lcall FT93C66_Read
			 mov time_refl_display, a

			 ret

			 Load_Defaults: ; Load defaults if keys are incorrect
				 mov temp_soak, #150
				 mov time_soak, #45
				 mov temp_refl, #225
				 mov time_refl, #30
				 ret

			 Save_Configuration2:
			     lcall FT93C66_Write_Enable
			     mov DPTR, #0x0050
			     ; Save variables
			     mov a, temp_soak
			     lcall FT93C66_Write
			     inc DPTR
			     mov a, time_soak
			     lcall FT93C66_Write
			     inc DPTR
			     mov a, temp_refl
			     lcall FT93C66_Write
			     inc DPTR
			     mov a, time_refl
			     lcall FT93C66_Write
			     inc DPTR

					 mov a, temp_soak_display
					 lcall FT93C66_Write
					 inc DPTR
					 mov a, time_soak_display
					 lcall FT93C66_Write
					 inc DPTR
					 mov a, temp_refl_display
					 lcall FT93C66_Write
					 inc DPTR
					 mov a, time_refl_display
					 lcall FT93C66_Write
inc DPTR
			     mov a, #0x55 ; First key value
			     lcall FT93C66_Write
			     inc DPTR
			     mov a, #0xAA ; Second key value
			     lcall FT93C66_Write
			     lcall FT93C66_Write_Disable
			     ret

				Load_Configuration2:
		   mov dptr, #0x0058 ;First key value location.  Must be 0x55
		   lcall FT93C66_Read
		   cjne a, #0x55, Load_Defaults
		   inc dptr ; Second key value location.  Must be 0xaa
		   lcall FT93C66_Read
		   cjne a, #0xaa, Load_Defaults

						mov dptr, #0x0050
					 lcall FT93C66_Read
					 mov temp_soak, a
					 inc dptr
					 lcall FT93C66_Read
					 mov time_soak, a
					 inc dptr
					 lcall FT93C66_Read
					 mov temp_refl, a
					 inc dptr
					 lcall FT93C66_Read
					 mov time_refl, a

					 inc dptr
					 lcall FT93C66_Read
					mov temp_soak_display, a
					inc dptr
					lcall FT93C66_Read
					mov time_soak_display, a
					inc dptr
					lcall FT93C66_Read
					mov temp_refl_display, a
					inc dptr
					lcall FT93C66_Read
					mov time_refl_display, a

					 ret

MainProgram:
  mov sp, #0x7f
	lcall Timer0_Init
  lcall Initialize_LEDs
  lcall Initialize_Serial_Port
  lcall Initialize_ADC
	lcall Initialize_oven

	mov P0MOD, #11111111B
	mov P1MOD, #00000011b ;configure P0.7
	mov P2MOD, #00001011b
	clr P0.7
	setb EA   ; Enable Global interrupts
	lcall ELCD_4BIT ; Configure LCD in four bit mode
	clr TR0

	; remove passcode protection
	ljmp Stuff

	mov dptr, #Startup_message
	lcall SendString
	Set_Cursor(1, 1)
	Send_Constant_String(#Startup_message)

Startup_screen:

	lcall getchar
	cjne a, #'0', Startup_screen
	lcall getchar
	cjne a, #'1', Startup_screen
	lcall getchar
	cjne a, #'9', Startup_screen
	lcall getchar
	cjne a, #'8', Startup_screen
	lcall getchar
	cjne a, #'4', Startup_screen

Stuff:
	Set_Cursor(1, 1)
  Send_Constant_String(#Temp_message)
	mov state, #0
	mov sec, #0
	mov runtime_min, #0x00
	mov runtime_sec, #0x00
	mov a, runtime_sec
	Set_Cursor(2,15)
	Display_BCD_(a)
	mov a, runtime_min
	Set_Cursor(2,12)
	Display_BCD_(a)
	Set_Cursor(2,14)
	WriteData(#':')

	mov SP, #7FH ; Set the stack pointer to the begining of idata
	clr FT93C66_CE
	lcall FT93C66_INIT_SPI;initialize the memory

	clr a
	clr long_beep_flag

	ljmp forever

STOPfirst_forever:
	ljmp STOPfirst

forever:
	lcall WaitHalfSec
	jnb KEY.1, STOPfirst_forever
	lcall WaitHalfSec
	jnb long_beep_flag, main
	cpl TR0
	clr long_beep_flag

main:
	mov a, #00000000B ; read the channel to convert from the switches
	anl a, #00000111B ; We need only the last three bits since there are only eight channels
	mov b, a
	lcall LTC2308_RW  ; Read the channel from the ADC
	lcall Calculate_Junction

	mov a, #00000001B ; read the channel to convert from the switches
	anl a, #00000111B ; We need only the last three bits since there are only eight channels
	mov b, a
	lcall LTC2308_RW  ; Read the channel from the ADC
	lcall Calculate_Temp

	mov temp, x
	mov temp+1,x+1

	lcall hex2bcd16   ; Convert to bcd
	lcall Display_BCD ; Display using the 7-segment displays
	lcall SendNumber  ; Send to serial port
	Send_BCD(BCD)

High_Temp_Warning:
	mov a, temp
	clr c
	subb a, #250
	jc main_2
	ljmp STOPsecond

main_2:
	mov a, sec
	add a, #0x01
	mov sec, a
	ljmp Time_disp

Time_disp:
	jb SWA.1, Temp_disp_jump

	Set_Cursor(1,1)
	Send_Constant_String(#Time_message)
	;Set_Cursor(2,10)
	;Send_Constant_String(#Time_passed)
	ljmp Time_disp_2

Temp_disp_jump:
	ljmp temp_disp

Time_disp_2:
	mov a, Time_soak_display
;	da a
	Set_Cursor(1,6)
  Display_BCD_(a)

	mov a, Time_refl_display
;	da a
	Set_Cursor(1,15)
	Display_BCD_(a)
  ljmp buttons

Buttons:
	jb KEY.2, Temp_disp_done_jump
	jnb KEY.2, $

	jnb SWA.1, time_change
	ljmp temp_change

Temp_disp_done_jump:
	ljmp Temp_disp_done

time_change:
	jnb SWA.2, increment_time
	jb SWA.2, decrement_time

increment_time:
	jnb SWA.3, time_soak_inc
	jb SWA.3, Time_refl_inc
	ljmp Temp_disp_done

time_soak_inc:
	mov a, Time_soak
	add a, #1
	mov Time_soak, a

	mov a, Time_soak_display
	add a, #1
	da a
	mov Time_soak_display, a
	ljmp Temp_disp_done

Time_refl_inc:
	mov a, Time_refl
	add a, #1
	mov Time_refl, a

	mov a, Time_refl_display
	add a, #1
	da a
	mov Time_refl_display, a
	ljmp Temp_disp_done

decrement_time:
	jnb SWA.3, time_soak_dec
	jb SWA.3, time_refl_dec
	ljmp Temp_disp_done

time_soak_dec:
	mov a, Time_soak
	subb a, #1
	mov Time_soak, a

	mov a, Time_soak_display
	add a, #0x99
	da a
	mov Time_soak_display, a

	ljmp Temp_disp_done

Time_refl_dec:
	mov a, Time_refl
	subb a, #1
	mov time_refl, a

	mov a, time_refl_display
	add a, #0x99
	da a
	mov Time_refl_display, a

	ljmp Temp_disp_done

temp_change:
	jnb SWA.2, increment_temp
	jb SWA.2, decrement_temp
;;;;;;;;;;;;;;;;;;;;;;;;;
increment_temp:
	jnb SWA.3, temp_soak_inc
	jb SWA.3, temp_refl_inc
	ljmp Temp_disp_done

temp_soak_inc:
	mov a, Temp_soak
	add a, #1
	mov temp_soak, a

	mov a, temp_soak_display
	add a, #1
	da a
	mov temp_soak_display, a
	ljmp Temp_disp_done

temp_refl_inc:
	mov a, Temp_refl
	add a, #1
	mov temp_refl, a

	mov a, temp_refl_display
	add a, #1
	da a
	mov Temp_refl_display, a
	ljmp Temp_disp_done

decrement_temp:
	jnb SWA.3, temp_soak_dec
	jb SWA.3, temp_refl_dec
	ljmp Temp_disp_done

temp_soak_dec:
	mov a, temp_soak
	subb a, #1
	mov temp_soak, a

	mov a, temp_soak_display
	add a, #0x99
	da a
	mov temp_soak_display, a

	ljmp Temp_disp_done

Temp_refl_dec:
	mov a, Temp_refl
	subb a, #1
	mov temp_refl, a

	mov a, temp_refl_display
	add a, #0x99
	da a
	mov temp_refl_display, a

	ljmp Temp_disp_done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Temp_disp:
	Set_Cursor(1,1)
	Send_Constant_String(#Temp_message)

	mov a, Temp_soak_display
	Set_Cursor(1,6)
	Display_BCD_(a)

	Set_cursor(1,5)
	Display_Least_BCD_(Temp_soak_display+1)

	Set_Cursor(1,15)
	mov a, (Temp_refl_display)
	Display_BCD_(a)

	Set_cursor(1,14)
	Display_Least_BCD_(Temp_refl_display+1)

	ljmp buttons

Temp_disp_done:
	mov a, state
	ljmp state0

state1_jmp:
	ljmp state1

state0:
  cjne a, #0, state1_jmp
  mov pwm, #0
  lcall nopower
	Set_Cursor(2,1)
	Send_Constant_String(#Off_Display)
				jnb SWA.5, print_read
				Set_Cursor(2,10)
				Send_Constant_String(#write)
				ljmp memory_start
				print_read:
				Set_Cursor(2,10)
				Send_Constant_String(#read)

				memory_start:
				;load to or read from the memory
				jnb SWA.6, memory_done
				jnb SWA.5, read_mode
				write_mode:
				Set_Cursor(2,10)
				Send_Constant_String(#write)
				jb SWA.4,LOC2
				lcall Save_Configuration1
				ljmp memory_done
				LOC2:
				lcall Save_Configuration2
				ljmp memory_done

				read_mode:
				Set_Cursor(2,10)
				Send_Constant_String(#read)
				jb SWA.4,LOC2_R
				lcall Load_Configuration1
				ljmp memory_done
				LOC2_R:
				lcall Load_Configuration2
				ljmp memory_done



	memory_done:
  jb KEY.3, state0_done
  jnb KEY.3, $ ; Wait for key release
  mov state, #1

	Set_Cursor(2,10)
	WriteData(#' ')

	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0

state0_done:
	lcall WaitQuartSec
  ljmp main

state1:
	mov a, runtime_sec
	cjne a, #0x59, inc_runtime_s
	clr a
	Set_Cursor(2,15)
	Display_BCD_(a)
	mov runtime_sec, a
	mov a, runtime_min
	add a, #0x01
	da a
	Set_Cursor(2,12)
	Display_BCD_(a)
	mov runtime_min, a
	mov a, state
	ljmp state1_2

inc_runtime_s:
	add a, #0x01
	da a
	Set_Cursor(2,15)
	Display_BCD_(a)
	mov runtime_sec, a
	mov a, runtime_min
	Set_Cursor(2,12)
	Display_BCD_(a)
	mov a, state
	ljmp state1_2

STOPfirst_1:
	ljmp STOPfirst

state1_2:
	jnb KEY.1, STOPfirst_1
  cjne a, #1, state2
	Set_Cursor(2,1)
	Send_Constant_String(#Preheat_Display)
  mov pwm, #100
  lcall fullpower

	mov a, runtime_min
	clr c
	subb a,#0x01
	jc state1_3

	mov a, temp
	clr c
	subb a,#50
	jc STOPsecond_jump
	ljmp state1_3

STOPsecond_jump:
	ljmp STOPsecond

state1_3:
  mov sec, #0
  mov a, temp_soak
  clr c
  subb a, temp
	jnc state1_done

  mov state, #2
	mov sec, #0
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0

state1_done:
  ljmp forever

STOPfirst_2:
	ljmp STOPfirst

state2:
	jnb KEY.1, STOPfirst_2
  cjne a, #2, state3
	Set_Cursor(2,1)
	Send_Constant_String(#Soak_Display)
  mov pwm, #20

  mov a, time_soak
  clr c
  subb a, sec

  jnc state2_done
  mov state, #3

	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0

state2_done:
  ljmp twenty_percent_power

STOPfirst_3:
	ljmp STOPfirst

state3:
	jnb KEY.1, STOPfirst_3
  cjne a, #3, state4
	Set_Cursor(2,1)
	Send_Constant_String(#Ramp_Display)
	mov pwm,#100
	lcall fullpower
	mov sec,#0
	mov a,Temp_refl
	clr c
	subb a,temp

	jnc state3_done

	mov sec,#0
  mov state, #4

	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0

state3_done:
  ljmp forever

STOPfirst_4:
	ljmp STOPfirst

state4:
	jnb KEY.1, STOPfirst_4
  cjne a, #4, state5
	Set_Cursor(2,1)
	Send_Constant_String(#Reflow_Display)
	mov pwm,#20
	mov a,Time_refl
	clr c
	subb a,sec

	jnc state4_done

  mov state, #5

	cpl TR0
	setb long_beep_flag

state4_done:
  ljmp twenty_percent_power

STOPfirst_5:
	ljmp STOPfirst

state5:
	jnb KEY.1, STOPfirst_5
  cjne a, #5, state0jmp
	Set_Cursor(2,1)
	Send_Constant_String(#Cooldown_Display)
	mov pwm,#0
	lcall nopower
	mov a,Temp_safe
	clr c
	subb a,temp

	jc state5_done

  mov state, #0

	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0

	ljmp state5_done

state0jmp:
	ljmp state0

state5_done:
  ljmp forever

nopower:
	clr P0.7
	ret

fullpower:
	setb P0.7
	ret

twenty_percent_power:
	setb P0.7
	jnb KEY.1, STOPfirst
	lcall WaitPointTwoSec
	clr P0.7
	jnb KEY.1, STOPfirst
	lcall WaitPointTwoSec
	jnb KEY.1, STOPfirst
	lcall WaitPointTwoSec
	jnb KEY.1, STOPfirst
	lcall WaitPointTwoSec
	jnb KEY.1, STOPfirst
	lcall WaitPointTwoSec
	jnb KEY.1, STOPfirst
	ljmp main

STOPfirst:
	jb KEY.1, $ ; Wait for key release
STOPsecond:
	mov pwm,#0
	lcall nopower
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	lcall WaitPointTwoSec
	cpl TR0
	Set_Cursor(2,1)
	Send_Constant_String(#STOP_Display)
	mov sec,#0
  mov state, #0
  ljmp STOPthird
STOPthird:
  lcall WaitHalfSec
  lcall WaitHalfSec
	ljmp STOPsecond

end
