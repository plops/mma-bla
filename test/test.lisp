(defpackage :ipms
  (:use :cl :sb-alien :sb-c-call))
 
(in-package :ipms)
 
(load-shared-object "/home/martin/linux-mma2/ipms_slm_shared_lib/64bit/libipms_slm.so.1.0.0")
 
;; all functions return 0 on success and <0 if there was an error
 
(define-alien-routine ("SLM_RegisterBoard" register-board)
    int
  (board-id unsigned-long-long)
  (ip4-addr c-string)
  (netmask c-string)
  (gateway c-string)
  (port unsigned-short))
 
(define-alien-routine ("SLM_SetLocalIf" set-local-interface)
    int
  (ip4-addr c-string)
  (port unsigned-short))
 
(define-alien-routine ("SLM_Connect" connect)
    int)
 
(define-alien-routine ("SLM_LoadConfiguration" load-configuration)
    int
  (filepath c-string))
 
(register-board #x0036344B00800803 "192.168.0.2" "255.255.255.0" "0.0.0.0" 4001)
(set-local-interface "192.168.0.1" 4001)
(connect)
(load-configuration "/home/martin/linux-mma2/boardini/800803.ini")
