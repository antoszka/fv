;;; This code is basically a (free) Polish translation of SBCL's
;;; implementation of the ~R format directive.
;;;
;;; TODO: ordinals are a work in progress. SBCL is published under the
;;; terms of the MIT license (and public domain where possible), and
;;; as such so is this file; original (c) by MIT, Symbolics, Xerox and
;;; Gerd Moellmann. Current modifications are mostly done by Adam
;;; Michalik/Dodek.

;;
(in-package #:fv)
;;

(defparameter *cardinal-ones*
  #(nil "jeden" "dwa" "trzy" "cztery" "pięć" "sześć" "siedem" "osiem" "dziewięć"))

(defparameter *cardinal-tens*
  #(nil nil "dwadzieścia" "trzydzieści" "czterdzieści"
        "pięćdziesiąt" "sześćdziesiąt" "siedemdziesiąt" "osiemdziesiąt" "dziewięćdziesiąt"))

(defparameter *cardinal-teens*
  #("dziesięć" "jedenaście" "dwanaście" "trzynaście" "czternaście"
    "piętnaście" "szesnaście" "siedemnaście" "osiemnaście" "dziewiętnaście"))

(defparameter *cardinal-hundreds*
  #("" "sto" "dwieście" "trzysta" "czterysta" "pięćset" "sześćset" "siedemset"
    "osiemset" "dziewiećset"))

(defparameter *cardinal-periods*
  #2A(("" "" "") 
      (" tysiąc" " tysiące" " tysięcy")
      (" milion" " miliony" " milionów")
      (" miliard" " miliardy" " miliardów")
      (" bilion" " biliony" " bilionów")
      (" trylion" " tryliony" " trylionów")
      (" kwadrylion" " kwadryliony" " kwadrylionów") 
      (" kwintylion" " kwintyliony" " kwintylionów") 
      (" sekstylion" " sekstyliony" " sekstylionów")
      (" septylion" " septyliony" " septylionów")
      (" oktylion" " oktyliony" " oktylionów")))


(defparameter *ordinal-ones-m*
  #(nil "pierwszy" "drugi" "trzeci" "czwarty"
        "piąty" "szósty" "siódmy" "ósmy" "dziewiąty"))
(defparameter *ordinal-ones-f*
  #(nil "pierwsza" "druga" "trzeca" "czwarta"
        "piąta" "szósta" "siódma" "ósma" "dziewiąta"))
(defparameter *ordinal-ones-n*
  #(nil "pierwsze" "drugie" "trzecie" "czwarte"
        "piąte" "szóste" "siódme" "ósme" "dziewiąte"))

(defparameter *ordinal-teens-m*
  #(nil "jedenasty" "dwunasty" "trzynasty" "czternasty" "piętnasty" "szesnasty"
        "siedemasty" "osiemnasty" "dziewiętnasty"))
(defparameter *ordinal-teens-f*
  #(nil "jedenasta" "dwunasta" "trzynasta" "czternasta" "piętnasta" "szesnasta"
        "siedemasta" "osiemnasta" "dziewiętnasta"))
(defparameter *ordinal-teens-n*
  #(nil "jedenaste" "dwunaste" "trzynaste" "czternaste" "piętnaste" "szesnaste"
        "siedemaste" "osiemnaste" "dziewiętnaste"))

(defparameter *ordinal-tens-m*
  #(nil "dziesiąty" "dwudziesty" "trzydziesty" "czterdziesty"
    "pięćdziesiąty" "sześćdziesiąty" "siedemdziesiąty" "osiemdziesiąty"
    "dziewięćdziesiąty"))
(defparameter *ordinal-tens-f*
  #(nil "dziesiąta" "dwudziesta" "trzydziesta" "czterdziesta"
    "pięćdziesiąta" "sześćdziesiąta" "siedemdziesiąta" "osiemdziesiąta"
    "dziewięćdziesiąta"))
(defparameter *ordinal-tens-n*
  #(nil "dziesiąte" "dwudzieste" "trzydzieste" "czterdzieste"
    "pięćdziesiąte" "sześćdziesiąte" "siedemdziesiąte" "osiemdziesiąte"
    "dziewięćdziesiąte"))

(defun format-print-small-cardinal (stream n)
  (multiple-value-bind (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref *cardinal-hundreds* hundreds) stream)
      (when (plusp rem)
        (write-char #\space stream)))
    (when (plusp rem)
      (multiple-value-bind (tens ones) (truncate rem 10)
        (cond ((< 1 tens)
              (write-string (svref *cardinal-tens* tens) stream)
              (when (plusp ones)
                (write-char #\space stream)
                (write-string (svref *cardinal-ones* ones) stream)))
             ((= tens 1)
              (write-string (svref *cardinal-teens* ones) stream))
             ((plusp ones)
              (write-string (svref *cardinal-ones* ones) stream)))))))

(defun format-print-cardinal (stream n)
  (cond ((minusp n)
         (write-string "minus " stream)
         (format-print-cardinal-aux stream (- n) 0 n))
        ((zerop n)
         (write-string "zero" stream))
        (t
         (format-print-cardinal-aux stream n 0 n))))


(defun format-print-cardinal-aux (stream n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (<= period 21)
      (error "zbyt duża liczba do wypisania po polsku: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond)
        (write-char #\space stream))
      (format-print-small-cardinal stream here)
      (format-print-period-name stream period here))))

(defun format-print-period-name (stream period cardinal)
  (multiple-value-bind (hundreds rest) (truncate cardinal 100)
    (declare (ignore hundreds))
    (multiple-value-bind (tens ones) (truncate rest 10)
      (write-string 
       (aref *cardinal-periods* period
             (cond ((= cardinal 1) 0)
                   ((or (= tens 1)
                        (= ones 0)
                        (= ones 1)
                        (>= ones 5)) 2)
                   (t 1)))
       stream))))
