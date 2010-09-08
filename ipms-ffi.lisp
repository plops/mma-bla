(defpackage :ipms-ffi
  (:use :cl :sb-alien :sb-c-call)
  (:export #:register-board
	   #:set-local-interface
	   #:connect
	   #:disconnect
	   #:load-configuration
	   #:set-voltage
	   #:get-voltage
	   #:load-picture
	   #:write-matrix-data
	   #:set-picture-sequence
	   #:set-power-on
	   #:set-power-off
	   #:set-stop-mma
	   #:set-start-mma
	   #:read-status
	   #:enable-extern-start
	   #:disable-extern-start
	   #:enable-microscope-mode
	   #:disable-microscope-mode
	   #:set-matrix-clock
	   #:get-matrix-clock
	   #:set-deflection-phase
	   #:get-deflection-phase
	   #:set-extern-ready
	   #:get-extern-ready
	   #:set-mma-temperature
	   #:get-mma-temperature
	   #:switch-peltier-on
	   #:switch-peltier-off
	   #:update-bitstream
	   #:update-embedded-software
	   #:+volt-frame-f+ 
	   #:+volt-frame-l+ 
	   #:+volt-dmd-f+ 
	   #:+volt-dmd-l+
	   #:+volt-shield-f+
	   #:+volt-shield-l+
	   #:+volt-ce-f+ 
	   #:+volt-ce-l+
	   #:+volt-pixel+ 
	   #:+volt-column-all+
	   ))

;; to generate list of functions in this file:
;; for i in `cat ipms-ffi.lisp|grep define-ali|cut -d " " -f 3|cut -d ")" -f 1`;do echo \#:$i;done


(in-package :ipms-ffi) 
(load-shared-object "/home/martin/linux-mma2/ipms_slm_shared_lib/64bit/libIPMS_SLM.so.1.0.0"
			 #+nil "/home/martin/linux-mma2/ipms_slm_shared_lib/32bit/libIPMS_SLM.so.1.0.0")
 
;; all functions return 0 on success and <0 if there was an error
 
(define-alien-routine ("SLM_RegisterBoard" register-board)
    int
  "64Bit BOARD-ID, IP4 address should have form 192.168.0.2 for example,
The port is used onboard for the UDP first setup communication and
[PORT+1] is the port number where the board starts its TCP Server
listing for incoming connections."
  (board-id unsigned-long-long)
  (ip4-addr c-string)
  (netmask c-string)
  (gateway c-string)
  (port unsigned-short))
 
(define-alien-routine ("SLM_SetLocalIf" set-local-interface)
    int
  "Outgoing network card where board is connected."
  (ip4-addr c-string)
  (port unsigned-short))
 
(define-alien-routine ("SLM_Connect" connect)
    int
  "Connect with board at given ID, setup with IP address and connect
to the onboard TCP Server.")

(define-alien-routine ("SLM_Disconnect" disconnect)
    int
  "Close socket connection with the board.")
 
(define-alien-routine ("SLM_LoadConfiguration" load-configuration)
    int
  "Load a board configuration and setup board with values."
  (filepath c-string))

(defconstant +volt-frame-f+ 0)
(defconstant +volt-frame-l+ 1)
(defconstant +volt-dmd-f+ 4)
(defconstant +volt-dmd-l+ 5)
(defconstant +volt-shield-f+ 6)
(defconstant +volt-shield-l+ 7)
(defconstant +volt-ce-f+ 8)
(defconstant +volt-ce-l+ 9)
(defconstant +volt-pixel+ 30)
(defconstant +volt-column-all+ 31)

(define-alien-routine ("SLM_SetVoltage" set-voltage)
    int
  "Sets a voltage by index (if you don't use load-configuration)
-l is for loading, -f for deflection phse:
volt-frame-f      0
volt-frame-l      1
volt-dmd-f        4
volt-dmd-l        5
volt-shield-f     6
volt-shield-l     7
volt-ce-f         8
volt-ce-l         9
volt-pixel       30
volt-column-all  31."
  (index unsigned-int)
  (value float))

(define-alien-routine ("SLM_GetVoltage" get-voltage)
    int
  "Get a voltage from library, may be used for UI programming.
For indizes see docstring of SET-VOLTAGE."
  (index unsigned-int)
  (value float :out))

(define-alien-routine ("SLM_LoadPicture" load-picture)
    int
  "Load a 24Bit bitmap (256x256) into PIC-NUMBER position in onboard
RAM, bitmap will be scaled by VPIX (Pixel voltage), PIC-NUMBER is in
range 1 to 1023, PIC-NUMBER 0 is reserved and should not be used."
  (file-path c-string)
  (pic-number unsigned-short))

(define-alien-routine ("SLM_WriteMatrixData" write-matrix-data)
    int
  "Transfer bitmap data without header [buftype=1, 256x256 BGR entries
per pixel] or short 16Bit buffers [buftype=3, 256x256, 16Bit entry per
pixel], for data orientation see the following schematic:
[ Green | Blue ]
[ 4..0  | 7..0 ]
[11..8  | 7..0 ] <- 12Bit output value for pixel."
  (pic-number unsigned-short)
  (buf-type unsigned-short)
  (buffer (* unsigned-short)) ;; void
  (buf-len unsigned-int))

(define-alien-routine ("SLM_SetPictureSequence" set-picture-sequence)
    int
  "PIC-NUMBER ranges from 1 to 1023
if LastPictureInSequence=0 then a new sequence is defined
LastPictureInSequence=1 actual PIC-NUMBER last picture in that sequence
READY-OUT-SIGNAL-NEEDED=0 suppress ReadyOut signal on SMB connector
[not needed for any synchronization]."
  (pic-number unsigned-short)
  (last-picture-in-sequence unsigned-short)
  (ready-out-signal-needed unsigned-short))

(define-alien-routine ("SLM_SetPowerOn" set-power-on)
    int
  "Turn MMA supplies on.")

(define-alien-routine ("SLM_SetPowerOff" set-power-off)
    int
  "Turn MMA supplies off.")

(define-alien-routine ("SLM_SetStopMMA" set-stop-mma)
    int
  "Stop MMA addressing.")

(define-alien-routine ("SLM_SetStartMMA" set-start-mma)
    int
  "Start MMA addressing.")

(define-alien-routine ("SLM_ReadStatus" read-status)
    int
  "Read board status [Power is On, MMA runs] and error status [MMA
ready signal failed -> defect MMA ?]."
  (status-mask unsigned-int :out)
  (error-mask unsigned-int :out))

(define-alien-routine ("SLM_EnableExternStart" enable-extern-start)
    int
  "Enable extern start trigger on SMB connector <Start> this is a flag
only and will become active with next SET-START-MMA command.")

(define-alien-routine ("SLM_DisableExternStart" disable-extern-start)
    int
  "Enable extern start trigger on SMB connector <Start> this is a flag
only and will become active with next SET-START-MMA command.")

(define-alien-routine ("SLM_EnableMicroscopeMode" enable-microscope-mode)
    int
  "Enables MicropscopeMode, [default board state is MicroscopeMode
disabled] -> improper use can damage MMA device!!")

(define-alien-routine ("SLM_DisableMicroscopeMode" disable-microscope-mode)
    int)

(define-alien-routine ("SLM_SetMatrixClock" set-matrix-clock)
    int
  "Setup matrix clock, default is [10MHz, 40ns delay, 50ns pulsewidth]
delay describes a delay between clock and data to ensure correct
takeover of voltages into pixel cells normally this settings doesn't
have to be changed."
  (freq-Mhz float)
  (delay-ns float)
  (pulse-width-ns float))

(define-alien-routine ("SLM_GetMatrixClock" get-matrix-clock)
    int
  "Get loaded values from library, maybe used for UI programming."
  (freq-Mhz float :out)
  (delay-ns float :out)
  (pulse-width-ns float :out))

(define-alien-routine ("SLM_SetDeflectionPhase" set-deflection-phase)
    int
  "Set values for deflection, DELAY-US is a gap after matrix signals
ready [<=16us], WIDTH-US is the length of the deflection phase where
flash voltages become active in MMA addressing."
  (delay-us float)
  (width-us float))

(define-alien-routine ("SLM_GetDeflectionPhase" get-deflection-phase)
    int
  (delay-us float :out)
  (width-us float :out))

(define-alien-routine ("SLM_SetExternReady" set-extern-ready)
    int
  "Set values for ready signal on SMB connector <Ready>. DELAY-US is a
gap after matrix signals ready, WIDTH-US defines the high
pulsewidth."
  (delay-us float)
  (width-us float))

(define-alien-routine ("SLM_GetExternReady" get-extern-ready)
    int
  "Get loaded values from library, may be used for UI programming."
  (delay-us float :out)
  (width-us float :out))

(define-alien-routine ("SLM_SetMMATemperature" set-mma-temperature)
    int
  "MMA target temperature for Peltier cooling system in degrees
celsius."
  (temperature-deg float))

(define-alien-routine ("SLM_GetMMATemperature" get-mma-temperature)
    int
  "Get loaded value from library, may be used for UI programming."
  (temperature-deg float :out))

(define-alien-routine ("SLM_SwitchPeltierOn" switch-peltier-on)
    int
  "Enable cooling system.")

(define-alien-routine ("SLM_SwitchPeltierOff" switch-peltier-off)
    int
  "Disable cooling system.")

(define-alien-routine ("SLM_UpdateBitstream" update-bitstream)
    int)

(define-alien-routine ("SLM_UpdateEmbeddedSW" update-embedded-software)
    int)
