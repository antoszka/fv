(in-package #:fv)
;;

(defparameter *company-data*
  (list
   :name "Nazwa firmy"
   :address "Ulica, nr"
   :postcode "Kod Pocztowy"
   :city "Miasto"
   :nip "NIP firmy"
   :email "firma@firma.tld"
   :account-pln "00 1111 2222 3333 4444 5555 6666"
   :account-usd "00 1111 2222 3333 4444 5555 6666"))

(defparameter *monthly-billed-clients*
  (list 'foo 
        'bar
        'baz
        'xyzzy))

(setf *fv-dir* (truename "~/invoices"))
