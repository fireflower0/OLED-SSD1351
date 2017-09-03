;; SPI settings
(defconstant +spi-cs+ 0)
(defconstant +spi-speed+ 10000000)

;; SSD1351 Commands
(defconstant +cmd-setcolumn+      #X15)
(defconstant +cmd-setrow+         #X75)
(defconstant +cmd-writeram+       #X5C)
(defconstant +cmd-readram+        #X5D)
(defconstant +cmd-setremap+       #XA0)
(defconstant +cmd-startline+      #XA1)
(defconstant +cmd-displayoffset+  #XA2)
(defconstant +cmd-displayalloff+  #XA4)
(defconstant +cmd-displayallon+   #XA5)
(defconstant +cmd-normaldisplay+  #XA6)
(defconstant +cmd-invertdisplay+  #XA7)
(defconstant +cmd-functionselect+ #XAB)
(defconstant +cmd-displayoff+     #XAE)
(defconstant +cmd-displayon+      #XAF)
(defconstant +cmd-precharge+      #XB1)
(defconstant +cmd-displayenhance+ #XB2)
(defconstant +cmd-clockdiv+       #XB3)
(defconstant +cmd-setvsl+         #XB4)
(defconstant +cmd-setgpio+        #XB5)
(defconstant +cmd-precharge2+     #XB6)
(defconstant +cmd-setgray+        #XB8)
(defconstant +cmd-uselut+         #XB9)
(defconstant +cmd-prechargelevel+ #XBB)
(defconstant +cmd-vcomh+          #XBE)
(defconstant +cmd-contrastabc+    #XC1)
(defconstant +cmd-contrastmaster+ #XC7)
(defconstant +cmd-muxratio+       #XCA)
(defconstant +cmd-commandlock+    #XFD)
(defconstant +cmd-horizscroll+    #X96)
(defconstant +cmd-stopscroll+     #X9E)
(defconstant +cmd-startscroll+    #X9F)

;; GPIO settings
(defconstant +pin-oled-cs+    8)
(defconstant +pin-oled-vcc+   18)
(defconstant +pin-oled-reset+ 23)
(defconstant +pin-oled-mosi+  10)
(defconstant +pin-oled-sclk+  11)

;; GPIO pin mode
(defconstant +output+ 1)
(defconstant +input+  0)

;; GPIO level
(defconstant +high+   1)
(defconstant +low+    0)

;; OLED
(defconstant +oled-cmd+    0)
(defconstant +oled-data+   1)
(defconstant +oled-width+  128)
(defconstant +oled-height+ 128)

;; Color
(defconstant +max-red+   #X1F)
(defconstant +max-green+ #X3F)
(defconstant +max-blue+  #X1F)
