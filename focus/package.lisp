(defpackage :serial
  (:shadowing-import-from :cl close open ftruncate truncate time)
  (:use :cl :sb-posix)
  (:export #:open-serial
	   #:close-serial
	   #:fd-type
	   #:serial-recv-length
	   #:read-response
	   #:write-zeiss
	   #:talk-zeiss))

(defpackage :focus
  (:use :cl :serial)
  (:export #:get-position
	   #:set-position
	   #:connect
	   #:disconnect))
