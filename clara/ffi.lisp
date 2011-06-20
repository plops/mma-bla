(in-package :clara)

(defparameter *clara-library* 
  (load-shared-object "/usr/local/lib/libandor.so.2.88.30003.0"))


(define-alien-routine ("GetAvailableCameras" get-available-cameras)
    unsigned-int
  (total-cameras int :out))
(define-alien-routine ("GetCameraHandle" get-camera-handle)
    unsigned-int
  (camera-index int)
  (camera-handle int :out))
(define-alien-routine ("SetCurrentCamera" set-current-camera)
    unsigned-int
  (camera-handle int))
(define-alien-routine ("Initialize" initialize)
    unsigned-int
  (dir c-string :in))
(define-alien-routine ("SetReadMode" set-read-mode)
    unsigned-int
  (read-mode int))
(define-alien-routine ("SetExposureTime" set-exposure-time)
    unsigned-int
  (time float))
(define-alien-routine ("GetDetector" get-detector)
    unsigned-int
  (xpixels int :out)
  (ypixels int :out))
(define-alien-routine ("SetShutter" set-shutter)
    unsigned-int
  (typ int)
  (shutter-mode int)
  (closing-time int)
  (opening-time int))
(define-alien-routine ("SetImage" set-image)
    unsigned-int
  (hbin int)
  (vbin int)
  (hstart int)
  (hend int)
  (vstart int)
  (vend int))
(define-alien-routine ("StartAcquisition" start-acquisition)
    unsigned-int)
(define-alien-routine ("PrepareAcquisition" prepare-acquisition)
    unsigned-int)
(define-alien-routine ("AbortAcquisition" abort-acquisition)
    unsigned-int)
(define-alien-routine ("GetStatus" get-status)
    unsigned-int
  (status int :out))
(define-alien-routine ("ShutDown" shutdown)
    unsigned-int)
(defconstant drv-success 20002)
(defconstant drv-acquiring 20072)
(defconstant drv-idle 20073)
(defconstant drv-tempcycle 20074)
(defconstant drv-not-initialized 20075)
(defconstant drv-no-new-data 20024)
(define-alien-routine ("GetAcquiredData" get-acquired-data)
    unsigned-int
  (arr (* int))
  (size unsigned-int))
(define-alien-routine ("GetAcquiredData16" get-acquired-data16)
    unsigned-int
  (arr (* unsigned-short))
  (size unsigned-long))
(define-alien-routine ("GetTotalNumberImagesAcquired" get-total-number-images-acquired)
    unsigned-int
  (index int :out))
(define-alien-routine ("GetSizeOfCircularBuffer" get-size-of-circular-buffer)
    unsigned-int
  (index int :out))

(define-alien-routine ("GetNumberNewImages" get-number-new-images)
    unsigned-int
  (first int :out)
  (last int :out))
(define-alien-routine ("GetNumberAvailableImages" get-number-available-images)
    unsigned-int
  (first int :out)
  (last int :out))
(define-alien-routine ("GetOldestImage" get-oldest-image)
    unsigned-int
  (arr (* int))
  (size unsigned-int))
(define-alien-routine ("GetOldestImage16" get-oldest-image16)
    unsigned-int
  (arr (* unsigned-short))
  (size unsigned-int))
(define-alien-routine ("GetMostRecentImage" get-most-recent-image)
    unsigned-int
  (arr (* int))
  (size unsigned-int))
(define-alien-routine ("GetMostRecentImage16" get-most-recent-image16)
    unsigned-int
  (arr (* unsigned-short))
  (size unsigned-int))

(define-alien-routine ("GetImages16" get-images16)
    unsigned-int
  (first long)
  (last long)
  (arr (* unsigned-short))
  (size unsigned-long)
  (validfirst long :out)
  (validlast long :out))

;; returns 4 values
(define-alien-routine ("GetAcquisitionTimings" get-acquisition-timings)
    unsigned-int
  (exposure float :out)
  (accumulate float :out)
  (kinetic float :out))
(define-alien-routine ("SetKineticCycleTime" set-kinetic-cycle-time)
    unsigned-int
  (time float))
(define-alien-routine ("SetAcquisitionMode" set-acquisition-mode)
    unsigned-int
  (acquisition-mode int))
(define-alien-routine ("GetNumberHSSpeeds" get-number-hs-speeds)
    unsigned-int
  (channel int)
  (typ int)
  (speeds int :out))
(define-alien-routine ("GetNumberVSSpeeds" get-number-vs-speeds)
    unsigned-int
  (speeds int :out))
(define-alien-routine ("GetNumberADChannels" get-number-ad-channels)
    unsigned-int
  (channels int :out))
(define-alien-routine ("GetNumberAmp" get-number-amp)
    unsigned-int
  (amp int :out))
(define-alien-routine ("GetNumberPreAmpGains" get-number-pre-amp-gains)
    unsigned-int
  (amp int :out))
(define-alien-routine ("SetADChannel" set-ad-channel)
    unsigned-int
  (channel int))
(define-alien-routine ("SetHSSpeed" set-hs-speed)
    unsigned-int
  (typ int)
  (index int))
(define-alien-routine ("SetVSSpeed" set-vs-speed)
    unsigned-int
  (index int))
(define-alien-routine ("GetBitDepth" get-bit-depth)
    unsigned-int
  (channel int)
  (depth int :out))
(define-alien-routine ("GetHSSpeed" get-hs-speed)
    unsigned-int
  (channel int)
  (typ int)
  (index int)
  (speed float :out))
(define-alien-routine ("GetVSSpeed" get-vs-speed)
    unsigned-int
  (index int)
  (speed float :out))
(define-alien-routine ("GetPreAmpGain" get-pre-amp-gain)
    unsigned-int
  (index int)
  (gain float :out))
(define-alien-routine ("IsPreAmpGainAvailable" is-pre-amp-gain-available)
    unsigned-int
  (channel int)
  (amplifier int)
  (index int)
  (pa int)
  (status int :out))

(define-alien-routine ("IsInternalMechanicalShutter" is-internal-mechanical-shutter)
    unsigned-int
  (internal-shutter int :out))
;;  (define-alien-routine ("GetCapabilities" get-capabilities)
;;     unsigned-int
;;   (caps (array unsigned-int 12) :out))
(define-alien-routine ("GetTemperature" get-temperature)
    unsigned-int
  (temp int :out))
(define-alien-routine ("GetTemperatureF" get-temperature-f) ;; detector in degrees celsius
    unsigned-int
  (temp float :out))
(define-alien-routine ("GetTemperatureRange" get-temperature-range)
    unsigned-int
  (mintemp int :out)
  (maxtemp int :out))
(define-alien-routine ("GetTemperatureStatus" get-temperature-status)
    unsigned-int
  (sensor-temp float :out)
  (target-temp float :out)
  (ambient-temp float :out)
  (cooler-volts float :out))
(define-alien-routine ("SetTemperature" set-temperature)
    unsigned-int
  (temp int))
(define-alien-routine ("CoolerON" cooler-on)
    unsigned-int)
(define-alien-routine ("CoolerOFF" cooler-off)
    unsigned-int)
(define-alien-routine ("FreeInternalMemory" free-internal-memory)
    unsigned-int)
(define-alien-routine ("IsCoolerOn" is-cooler-on)
    unsigned-int
  (status int :out))
(define-alien-routine ("SetOutputAmplifier" set-output-amplifier)
    unsigned-int
  (typ int))
(define-alien-routine ("SetFrameTransferMode" set-frame-transfer-mode)
    unsigned-int
  (frame-transfer-mode int))
(define-alien-routine ("SetTriggerMode" set-trigger-mode)
    unsigned-int
  (trigger-mode int))
(define-alien-routine ("IsTriggerModeAvailable" is-trigger-mode-available)
    unsigned-int
  (trigger-mode int))
(define-alien-routine ("GetAcquisitionProgress" get-acquisition-progress)
    unsigned-int
  (acc long :out)  ;; number of accumulation and series scans completed
  (series long :out)) ;; number of kinetic scans completed
 
(define-alien-routine ("WaitForAcquisition" wait-for-acquisition)
    unsigned-int)
 
(define-alien-routine ("SetNumberAccumulations" set-number-accumulations)
    unsigned-int
  (number int))

(define-alien-routine ("SetDACOutputScale" set-dac-output-scale)
    unsigned-int
  (scale int)) ;; only clara
 
;; either use get-number-new-images and get-images or use
;; get-most-recent-image (or get-oldest-image)
(defparameter *w* 0)
(defparameter *h* 0)
 
;; cat /home-old/martin/src/andor/include/atmcdLXd.h |grep define|grep DRV_|awk '{print "("$3+1-1 " '\''" $2")"}'
(defun lookup-error (err)
  (ecase err
    (20001 'DRV_ERROR_CODES)
    (20002 'DRV_SUCCESS)
    (20003 'DRV_VXDNOTINSTALLED)
    (20004 'DRV_ERROR_SCAN)
    (20005 'DRV_ERROR_CHECK_SUM)
    (20006 'DRV_ERROR_FILELOAD)
    (20007 'DRV_UNKNOWN_FUNCTION)
    (20008 'DRV_ERROR_VXD_INIT)
    (20009 'DRV_ERROR_ADDRESS)
    (20010 'DRV_ERROR_PAGELOCK)
    (20011 'DRV_ERROR_PAGEUNLOCK)
    (20012 'DRV_ERROR_BOARDTEST)
    (20013 'DRV_ERROR_ACK)
    (20014 'DRV_ERROR_UP_FIFO)
    (20015 'DRV_ERROR_PATTERN)
    (20017 'DRV_ACQUISITION_ERRORS)
    (20018 'DRV_ACQ_BUFFER)
    (20019 'DRV_ACQ_DOWNFIFO_FULL)
    (20020 'DRV_PROC_UNKONWN_INSTRUCTION)
    (20021 'DRV_ILLEGAL_OP_CODE)
    (20022 'DRV_KINETIC_TIME_NOT_MET)
    (20023 'DRV_ACCUM_TIME_NOT_MET)
    (20024 'DRV_NO_NEW_DATA)
    (20026 'DRV_SPOOLERROR)
    (20027 'DRV_SPOOLSETUPERROR)
    (20028 'DRV_FILESIZELIMITERROR)
    (20029 'DRV_ERROR_FILESAVE)
    (20033 'DRV_TEMPERATURE_CODES)
    (20034 'DRV_TEMPERATURE_OFF)
    (20035 'DRV_TEMPERATURE_NOT_STABILIZED)
    (20036 'DRV_TEMPERATURE_STABILIZED)
    (20037 'DRV_TEMPERATURE_NOT_REACHED)
    (20038 'DRV_TEMPERATURE_OUT_RANGE)
    (20039 'DRV_TEMPERATURE_NOT_SUPPORTED)
    (20040 'DRV_TEMPERATURE_DRIFT)
    (20049 'DRV_GENERAL_ERRORS)
    (20050 'DRV_INVALID_AUX)
    (20051 'DRV_COF_NOTLOADED)
    (20052 'DRV_FPGAPROG)
    (20053 'DRV_FLEXERROR)
    (20054 'DRV_GPIBERROR)
    (20055 'DRV_EEPROMVERSIONERROR)
    (20064 'DRV_DATATYPE)
    (20065 'DRV_DRIVER_ERRORS)
    (20066 'DRV_P1INVALID)
    (20067 'DRV_P2INVALID)
    (20068 'DRV_P3INVALID)
    (20069 'DRV_P4INVALID)
    (20070 'DRV_INIERROR)
    (20071 'DRV_COFERROR)
    (20072 'DRV_ACQUIRING)
    (20073 'DRV_IDLE)
    (20074 'DRV_TEMPCYCLE)
    (20075 'DRV_NOT_INITIALIZED)
    (20076 'DRV_P5INVALID)
    (20077 'DRV_P6INVALID)
    (20078 'DRV_INVALID_MODE)
    (20079 'DRV_INVALID_FILTER)
    (20080 'DRV_I2CERRORS)
    (20081 'DRV_I2CDEVNOTFOUND)
    (20082 'DRV_I2CTIMEOUT)
    (20083 'DRV_P7INVALID)
    (20084 'DRV_P8INVALID)
    (20085 'DRV_P9INVALID)
    (20086 'DRV_P10INVALID)
    (20089 'DRV_USBERROR)
    (20090 'DRV_IOCERROR)
    (20091 'DRV_VRMVERSIONERROR)
    (20093 'DRV_USB_INTERRUPT_ENDPOINT_ERROR)
    (20094 'DRV_RANDOM_TRACK_ERROR)
    (20095 'DRV_INVALID_TRIGGER_MODE)
    (20096 'DRV_LOAD_FIRMWARE_ERROR)
    (20097 'DRV_DIVIDE_BY_ZERO_ERROR)
    (20098 'DRV_INVALID_RINGEXPOSURES)
    (20099 'DRV_BINNING_ERROR)
    (20100 'DRV_INVALID_AMPLIFIER)
    (20990 'DRV_ERROR_NOCAMERA)
    (20991 'DRV_NOT_SUPPORTED)
    (20992 'DRV_NOT_AVAILABLE)
    (20115 'DRV_ERROR_MAP)
    (20116 'DRV_ERROR_UNMAP)
    (20117 'DRV_ERROR_MDL)
    (20118 'DRV_ERROR_UNMDL)
    (20119 'DRV_ERROR_BUFFSIZE)
    (20121 'DRV_ERROR_NOHANDLE)
    (20130 'DRV_GATING_NOT_AVAILABLE)
    (20131 'DRV_FPGA_VOLTAGE_ERROR)
    (20150 'DRV_OW_CMD_FAIL)
    (20151 'DRV_OWMEMORY_BAD_ADDR)
    (20152 'DRV_OWCMD_NOT_AVAILABLE)
    (20153 'DRV_OW_NO_SLAVES)
    (20154 'DRV_OW_NOT_INITIALIZED)
    (20155 'DRV_OW_ERROR_SLAVE_NUM)
    (20156 'DRV_MSTIMINGS_ERROR)
    (t err)))
 
#+nil
(lookup-error 20075)


(define-alien-routine ("GetCameraInformation" get-camera-information)
    unsigned-int
  (index int)
  (information long :out))

(define-alien-routine ("GetCameraSerialNumber" get-camera-serial-number)
    unsigned-int
  (number int :out))


(define-alien-routine ("GetCapabilities" get-capabilities)
    unsigned-int
  (caps (* unsigned-int)))

(define-alien-routine ("GetHeadModel" get-head-model)
    unsigned-int ;; char array should have length MAX_PATH which isn't defined
  (name (* char)))

(define-alien-routine ("SetFanMode" set-fan-mode)
    unsigned-int ;; full=0, low=1, off=2
  (mode int))

(define-alien-routine ("SetAdvancedTriggerModeState" set-advanced-trigger-mode-state)
    unsigned-int
  (icam int))

(define-alien-routine ("SaveAsSif" save-as-sif)
    unsigned-int
  (path c-string))

(define-alien-routine ("SetFastExtTrigger" set-fast-external-trigger)
    unsigned-int
  (mode int))

(define-alien-routine ("SetIsolatedCropMode" set-isolated-crop-mode)
    unsigned-int
  (active int)
  (height int)
  (width int)
  (vbin int)
  (hbin int))

(define-alien-routine ("SetOverlapMode" set-overlap-mode)
    unsigned-int
  (mode int))