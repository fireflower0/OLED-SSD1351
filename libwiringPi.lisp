(define-foreign-library libwiringPi
  (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;;;; Core function

;; Initialization of the wiringPi
(defcfun "wiringPiSetupGpio" :int)

;; Set the mode of the GPIO pin
(defcfun "pinMode" :void (pin :int) (mode :int))

;; GPIO pin output control
(defcfun "digitalWrite" :void (pin :int) (value :int))

;; Waiting process
(defcfun "delay" :void (howlong :uint))

;; Set the state when nothing is connected to the terminal
(defcfun "pullUpDnControl" :void (pin :int) (pud :int))

;; Read the status of the GPIO pin
(defcfun "digitalRead" :int (pin :int))

;;;; I2C Library

;; Initialization of the I2C systems.
(defcfun "wiringPiI2CSetup" :int (fd :int))

;; Writes 8-bit data to the instructed device register.
(defcfun "wiringPiI2CWriteReg8" :int (fd :int) (reg :int) (data :int))

;; It reads the 16-bit value from the indicated device register.
(defcfun "wiringPiI2CReadReg16" :int (fd :int) (reg :int))

;;;; SPI library

;; SPI初期化
(defcfun "wiringPiSPISetup" :int (channel :int) (speed :int))

;; SPI初期化(Mode設定)
(defcfun "wiringPiSPISetupMode" :int (channel :int) (speed :int) (mode :int))

;; 選択されたSPIバス上での同時書込み／読出しトランザクションを実行
(defcfun "wiringPiSPIDataRW" :int (channel :int) (data :pointer) (len :int))

