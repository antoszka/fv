;;; this is a part of fv, the invoicing program

(in-package #:fv)

;;
;; some q-and-a wizards and helper functions for creating clients and invoices
;;

(defun ask (question)
  (format T "~a" question)
  (read-line))

(defun nullify (string)
  (if (= (length string) 0)
      nil
      string))

(defun list-clients ()
  (dolist (client (getf *db* :client))
    (let ((nick (getf client :nick))
          (name (getf client :name)))
      (format t "~&~a – ~a" nick name))))

(defun select-client ()
  (list-clients)
  (intern (string-upcase (ask "Choose client by symbol: ")) :fv))

(defun qa-add-client ()
  (format t "Creating client...~&")
  (let ((name         (ask "Name: "))
        (address      (ask "Street address: "))
        (postcode     (ask "Postcode: "))
        (city         (ask "City: "))
        (nip          (ask "NIP (NNNNNNNNNN): "))
        (email        (ask "E-mail: "))
        (nick         (ask "Nick (symbol): "))
        (default-item (ask "Default item (symbol) [nil]: "))
        (payment-days (ask "Payment days (N) [7]: ")))
    (let ((client (make-client :name         name
                               :address      address
                               :postcode     postcode
                               :city         city
                               :nip          (parse-integer nip)
                               :email        email
                               :nick         (intern (string-upcase nick) :fv)
                               :default-item (intern (string-upcase (nullify default-item)) :fv)
                               :payment-days (or (parse-integer payment-days :junk-allowed t) 7))))
      (terpri) (print client) (terpri)
      (if (yes-or-no-p "Client OK? Add to DB?")
          (progn
            (read-db)
            (add-to-db client)
            (write-db))
          (when (yes-or-no-p "Start with client again?")
            (qa-add-client))))))

(defun qa-collect-items ()
  (let ((items nil))
    (labels ((qa-item ()
               (format t "Creating item...~&")
               (let ((title (ask "Title: "))
                     (vat   (ask "VAT (23|8|5|zw) [zw]: "))
                     (count (ask "Count [1]: "))
                     (net   (ask "Net value: "))
                     (nick  (ask "Nick (symbol): ")))
                 (let ((item (make-item :title title
                                        :vat   (or (parse-integer vat :junk-allowed t) "zw")
                                        :count (or (parse-integer count :junk-allowed t) 1)
                                        :net   (read-from-string net)
                                        :nick  (intern (string-upcase nick) :fv))))
                   (terpri) (print item) (terpri)
                   (when (yes-or-no-p "Item OK? Push on list?")
                     (push item items))
                   (when (yes-or-no-p "Add another item?")
                     (qa-item))))))
      (qa-item))
    items))

(defun qa-add-invoice ()
  (let ((invoice
         (make-invoice :client (select-by-nick :client (select-client))
                       :items  (qa-collect-items)
                       :payment-days
                       (or (parse-integer (ask "Payement days (N) [7]: ") :junk-allowed t) 7))))
    (terpri) (print invoice) (terpri)
    (if (yes-or-no-p "Invoice OK? Add to DB?")
        (progn
          (read-db)
          (add-to-db invoice)
          (write-db)
          (when (yes-or-no-p "Print invoice?")
            (print-invoice invoice :mail (yes-or-no-p "E-mail invoice, too?"))))
        (when (yes-or-no-p "Start with invoice again?")
          (qa-add-invoice)))))
