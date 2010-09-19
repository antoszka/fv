;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; f.lisp – (c) 2010 Antoni Grzymała
;;;
;;; This is my personal invoicing program, might only ever be useful
;;; in the Polish VAT-invoice area.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for now, the useful public interface of this is limited to:
;;; (add-to-db) used in conjunction with:
;;;   (make-client)
;;;   (make-item)
;;;   (make-invoice) used in conjunction with:
;;;     (select-by-nick ...) or
;;;     (adjust-key (select-by-nick ...))
;;;
;;; (read-db)
;;; (write-db)
;;; (print-invoice) in conjunction with:
;;;   (select-invoice-by-id)
;;;
;;; remaning are helper functions and similar crap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO:
;;; * removing entries from database
;;; * selecting invoices by certain criteria
;;; * interactive mode (text UI) with browsing clients/items
;;; * command-line switches for scripting
;;; * corrective invoices
;;;
;;; --- later:
;;; * curses/tk/clim GUI
;;; * webgui
;;; * client/server (with android client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXAMPLES:
;;; (make-invoice :client (select-by-nick :client 'asp) :items (list (select-by-nick :item 'adminowanie-asp)))
;;; (make-invoice :client (select-by-nick :client 'asp) :items (list (adjust-key (select-by-nick 'adminowanie-asp :item) :count 2)
;;; 								     (select-by-nick :item 'coś-innego)))
;;; (add-to-db (↑))

;;;
;;; clhs appendf (NOT USED)
;;;

;; (define-modify-macro appendf (&rest args)
;;  append "Append onto list")

;;;
;;; load Dodek's code for translating numbers to Polish:
;;;

(defvar *program-directory*
  (make-pathname
   :directory '(:absolute "home" "antoni" "fv")))

;;; We probably don't need the snippet below, as we've set up an asdf system.
;;;
;;; (load (compile-file (merge-pathnames
;;;		     *program-directory*
;;;		     (make-pathname :name "polish" :type "lisp"))))

;;; initialize the database and set default database filename:

(defvar *db* (list :item () :client () :invoice ()))
(defvar *db-file* (merge-pathnames (user-homedir-pathname) #P".fv.db"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some needed functions
;;;

;;;
;;; following make-* functions return plists with our main types
;;; (item, client, invoice):
;;;

(defun make-item (&key title (vat 22) (count 1) net nick)
  "Create a single item inventory entry with a title, VAT, net price and nickname."
  (list
   :type  'item ; ← this tells us this is an item *db* entry
   :title title
   :vat   vat
   :count count
   :net   net
   :nick  nick))

(defun make-client (&key name address postcode (city "Warszawa")
		    nip email nick (payment-days 7))
  "Create a client inventory item with name, address, postcode, city, nip, e-mail and nick"
  (list
   :type         'client ; ← this tells us this is a client *db* entry
   :name         name
   :address      address
   :postcode     postcode
   :city         city
   :nip          nip
   :email        email
   :nick         nick
   :payment-days payment-days)) ; ← 0 means we want cash (7 is default)

(defun make-invoice (&key client items (payment-days 7)
		     year month date number)
  "Create an invoice with client, item list and date, payment type (not nil for cash) and payment days."
  (let* ((universal-time (get-universal-time))
	 (invoice-year   (or year   (getdate 'year  universal-time)))
	 (invoice-month  (or month  (getdate 'month universal-time)))
	 (invoice-date   (or date   (getdate 'date  universal-time)))
	 (invoice-number (or number (get-highest-number :for-month invoice-month :for-year invoice-year)))
	 (invoice-id     (format nil "FV-~d/~d/~d" invoice-number invoice-month invoice-year)))
    ;; the above is to avoid a race-condition and make sure we create
    ;; a date in an atomic operation
    (when (not (listp (elt items 0)))
      (error "Item list should be a nested list"))
    (list
     :type         'invoice
     :client       client
     :items        items
     :year         invoice-year
     :month        invoice-month
     :date         invoice-date
     :number       invoice-number
     :id           invoice-id
     :payment-days payment-days))) ; ← 0 means we want cash

;;;
;;; return nearest possible invoice number (for a given month/year)
;;;

(defun get-highest-number (&key for-month for-year)
  "Return next usable invoice number for a given month/year based upon what's in the db."
  (let ((current-highest 0)
	(m for-month)
	(y for-year))
    (dolist (k (getf *db* :invoice))
      (cond ((and (eql (getf k :month) m)
		  (eql (getf k :year)  y))
	     (if (< current-highest (getf k :number))
		 (setf current-highest (getf k :number))
		 current-highest))))
    (1+ current-highest)))

;;;
;;; return some interesting date elements
;;;

(defun getdate (what-we-want universal-time)
  "Return a decoded date element."
  (nth-value
   (position what-we-want
	     '(second minute hour date month year day-of-week dst-p tz))
   (decode-universal-time universal-time)))

;;;
;;; return a database entry based upon nick and entry group (or an invoice by id)
;;; TODO – maybe consolidate the two, they're practically identical
;;;

(defun select-by-nick (group nick)
  "Returns an item or client matched by nick."
  (dolist (k (getf *db* group))
    (if (eql (getf k :nick) nick)
	(return k)
	nil)))

(defun select-invoice-by-id (id)
  "Returns an invoice when id is matched in the db."
  (dolist (k (getf *db* :invoice))
    (if (equal (getf k :id) id) ; equal!, we're comparing strings!
	(return k)
	nil)))

;;;
;;; function to adjust in-flight a key of a plist (with the intention
;;; of modifying :count when adding an item to an invoice
;;;

(defun adjust-key (plist key value)
  "Modify or add a field in a plist in place"
  (setf (getf plist key) value)
  plist)

;;;
;;; check if NIP identification number is correct and non-nil
;;;

(defun correct-nip-p (nip)
  (if (let ((nip-string (format nil "~d" nip)))
	(when (= (length nip-string) 10)
	  (let ((checksum (loop for w in '(6 5 7 2 3 4 5 6 7)
			     for i across nip-string
			     sum (* w (digit-char-p i)))))
	    (= (digit-char-p (elt nip-string 9)) (rem checksum 11)))))
      nil t))

;;;
;;; add something to our database tree:
;;;

(defun add-to-db (entry)
  "Add anything to the db (in the right place) and do some verification tests."
  (let ((nick   (getf entry :nick))
	(nip    (getf entry :nip))
	(type   (getf entry :type))
	(id     (getf entry :id)))
    (cond
      ((equal type 'item)
       (when (or (select-by-nick nick :item) (not nick))
	 (error "~S already exists as an item nick or nick empty." nick))
       (push entry (getf *db* :item)))
      ((equal type 'client)
       (when (or (select-by-nick nick :client) (not nick))
	 (error "~S already exists as an item nick or nick empty." nick))
       (when (or (correct-nip-p nip) (not nip))
	 (error "~S is not a correct NIP number." nip))
       (push entry (getf *db* :client)))
      ((equal type 'invoice)
       (when (or (select-invoice-by-id id) (not id))
	 (error "~S already exists as an invoice id." id))
       (push entry (getf *db* :invoice)))
      (t nil))))

;;;
;;; dump stuff function (DEBUG)
;;;

(defun dump-db (type)
  "Quick view of the db (selected by type)"
  (dolist (db-entry (getf *db* type))
    (format t "~{~a:~10t~a~%~}~%" db-entry)))

;;;
;;; saving and loading the db
;;;

(defun write-db (&optional (pathname *db-file*))
  "Saves the invoice/client/item database to a file."
  (with-open-file (output pathname
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (write *db* :case :downcase :pretty t :stream output) ; pretty-print database contents
      (format output "~%"))))                               ; add trailing newline

(defun read-db (&optional (pathname *db-file*))
  "Loads the invoice/client/item database from a file"
  (with-open-file (input pathname
			 :direction :input)
    (with-standard-io-syntax
      (setf *db* (read input)))))

;;;
;;; calculate all necessary invoice fields
;;;

(defun calculate-invoice-fields (invoice)
  "Returns a plist with calculations of various invoice fields needed for invoice visualisation and printout."
  (let* ((gross-total       0) ;; do we need to declare all those here?
	 (gross-total-int   0)
	 (gross-total-cent  0)
	 (net-total         0)
	 (vat-total         0)
	 (words-gross-total "")
	 (payment-days      (getf (getf invoice :client) :payment-days))
	 (invoice-date      (getf invoice :date))
	 (invoice-month     (getf invoice :month))
	 (invoice-year      (getf invoice :year))
	 (payment-form      "")
	 (22-net-total      0)
	 (22-vat-total      0)
	 (22-gross-total    0)
	 (7-net-total       0)
	 (7-vat-total       0)
	 (7-gross-total     0)
	 (3-net-total       0)
	 (3-vat-total       0)
	 (3-gross-total     0)
	 (zw-net-total      0)
	 (item-position     1)
	 (calculated-items  nil))
    (dolist (item (getf invoice :items)) ;; ← *should* be a list
      (let* ((item-vat       (getf item :vat))
	     (item-count     (getf item :count))
	     (item-title     (getf item :title))
	     (item-net       (getf item :net))
	     (vat-multiplier (/ (if (equal item-vat "zw") ;; the "zw" (zwolniony)
				    0                     ;; VAT rate is
				    item-vat)             ;; effectively 0%
				100)))                    ;; TODO – check if interger here OK
	(cond ((equal item-vat 22)  
	       (incf 22-net-total   (* item-count item-net))
	       (incf 22-vat-total   (* item-count item-net 0.22))
	       (incf 22-gross-total (* item-count item-net 1.22)))
	      ((equal item-vat 7)   
	       (incf 7-net-total    (* item-count item-net))
	       (incf 7-vat-total    (* item-count item-net 0.07))
	       (incf 7-gross-total  (* item-count item-net 1.07)))
	      ((equal item-vat 3)
	       (incf 3-net-total    (* item-count item-net))
	       (incf 3-vat-total    (* item-count item-net 0.03))
	       (incf 3-gross-total  (* item-count item-net 1.03)))
	      ((equal item-vat "zw") 
	       (incf zw-net-total   (* item-count item-net))))
	(push (list item-position
	            item-title
		    (polish-monetize item-net)
		    item-count
		    (polish-monetize (* item-net item-count))
		    (if (equal item-vat "zw") "zw."
			(format nil "~d\\%" item-vat))
		    (polish-monetize (* item-net item-count vat-multiplier))
		    (polish-monetize (* item-net item-count (1+ vat-multiplier))))
	      calculated-items)
	(incf item-position)))
    (setq gross-total (+ 22-gross-total 7-gross-total 3-gross-total zw-net-total))
    (setq net-total   (+ 22-net-total   7-net-total   3-net-total   zw-net-total))
    (setq vat-total   (+ 22-vat-total   7-vat-total   3-vat-total))
    (multiple-value-bind (int cent)
	(floor (read-from-string (format nil "~$" gross-total)))
      (setq gross-total-int  int)
      (setq gross-total-cent (floor (* cent 100))))
    (setq words-gross-total (with-output-to-string (words)
			      (format-print-cardinal words gross-total-int)
			      (format words " ~a"
				      (multiple-value-bind (tens ones)
					  (truncate gross-total-int 10)
					(if (and (= tens 0) (= ones 1))
					    "złoty"
					    (case ones
					      ((2 3 4) "złote")
					      (otherwise "złotych")))))))
    (setq payment-form
	  (if (<= payment-days 0)
	      "Płatne gotówką."
	      (multiple-value-bind (a b c day month year d e f)
		  (decode-universal-time
		   (+ (* payment-days 86400)
		      (encode-universal-time
		       0 0 0 invoice-date invoice-month invoice-year)))
		(declare (ignore a b c d e f))
		(format nil
			"Płatne przelewem do dnia: ~d/~d/~d (~d dni)."
			day month year payment-days))))
    ;; now we return all we calculated in a single plist:
    (list :gross-total       (polish-monetize gross-total)
	  :gross-total-int   gross-total-int
	  :gross-total-cent  gross-total-cent
	  :net-total         (polish-monetize net-total)
	  :vat-total         (polish-monetize vat-total)
	  :words-gross-total words-gross-total
	  :payment-days      payment-days
	  :invoice-date      invoice-date
	  :invoice-month     invoice-month
	  :invoice-year      invoice-year
	  :payment-form      payment-form
	  :22-net-total      (polish-monetize 22-net-total)
	  :22-vat-total      (polish-monetize 22-vat-total)
	  :22-gross-total    (polish-monetize 22-gross-total)
	  :7-net-total       (polish-monetize 7-net-total)
	  :7-vat-total       (polish-monetize 7-vat-total)
	  :7-gross-total     (polish-monetize 7-gross-total)
	  :3-net-total       (polish-monetize 3-net-total)
	  :3-vat-total       (polish-monetize 3-vat-total)
	  :3-gross-total     (polish-monetize 3-gross-total)
	  :zw-net-total      (polish-monetize zw-net-total)
	  :calculated-items  calculated-items)))

;;;
;;; Convert a float to a string with 2 decimal places, and a decimal comma
;;;

(defun polish-monetize (value)
  (let* ((reversed-string (reverse (substitute #\, #\. (format nil "~$" value))))
	 (reversed-length (length reversed-string)))
    (reverse
     (concatenate 'string 
		  (loop for char across reversed-string
		     counting char into count
		     collect char
		     when (and (eq 0 (mod count 3))
			       (> count 5)
			       (< count reversed-length)) collect #\.)))))

;;;
;;; printing an invoice
;;; date calculation hint:
;;;

(defun print-invoice (invoice)
  "Creates a tex printout file of a given invoice by means of executing emb code in a tex template."
  (let* ((env-plist
	  (list :invoice-id        (getf invoice :id)
		:invoice-date-full (format nil "~a/~a/~a"
					   (getf invoice :date)
					   (getf invoice :month)
					   (getf invoice :year))
		:buyer-name        (getf (getf invoice :client) :name)
		:buyer-address     (getf (getf invoice :client) :address)
		:buyer-postcode    (getf (getf invoice :client) :postcode)
		:buyer-nip         (getf (getf invoice :client) :nip)
		:item-list         (getf invoice :items)))
	 (output-filename (merge-pathnames
			   (user-homedir-pathname)
			   (format nil "fv-~d-~d-~d-~a.tex"
				   (getf invoice :year)
				   (getf invoice :month)
				   (getf invoice :number)
				   (getf (getf invoice :client) :nick)))))
    (emb:register-emb "template" (merge-pathnames *program-directory*
						  (make-pathname :name "emb-template"
								 :type "tex")))
    (with-open-file (output (merge-pathnames (user-homedir-pathname) output-filename)
			    :direction :output
			    :if-exists :supersede)
      (format output "~a"
	      (emb:execute-emb "template"
			       :env (append env-plist (calculate-invoice-fields invoice)))))))
