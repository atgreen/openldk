;;;; Wonderland Books: order processing
;;;;
;;;; Reads an order file, prices it, and prints an invoice and some
;;;; recommendations.  The application is Common Lisp; the pieces are
;;;; written in whichever JVM language suits them, all running on OpenLDK
;;;; in this one SBCL process:
;;;;
;;;;   Kotlin   the catalog, order-line parsing and the pricing engine
;;;;            (catalog.kt, compiled by ASDF)
;;;;   JScheme  ISBN-13 check digits
;;;;   Scala    the discount rules, as a pattern match
;;;;   Kawa     finding an in-stock substitute, escaping the search with call/cc
;;;;   Clojure  "customers who bought this also bought" recommendations
;;;;   Groovy   the invoice template, wrapping titles with commons-text (Maven)
;;;;   ABCL     the amount in words, with FORMAT
;;;;
;;;; Each foreign piece is written inline, in its own language's syntax,
;;;; in a #@LANGUAGE{ ... } block (or #[ ... ] for Clojure); $x passes a
;;;; Lisp value in.  The comments point out the OpenLDK features each part
;;;; relies on.
;;;;
;;;;   (asdf:load-system "polyglot-showcase")
;;;;   (polyglot-showcase:run)                 ; the bundled order-1042.txt
;;;;   (polyglot-showcase:run #p"my-order.txt")

(defpackage #:polyglot-showcase
  (:use #:cl #:openldk.interop)
  (:nicknames #:showcase)
  (:export #:run))

(in-package #:polyglot-showcase)

;;; Every language is started while this file compiles, not just when it
;;; runs: a #@kt{ } block whose splices have static types is then compiled
;;; together with this file, and its class files go into the fasl (see
;;; ROUND-CENTS).  ENABLE-SYNTAX turns on #@LANGUAGE{ ... } and Clojure's
;;; #[ ... ] for the rest of the file.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (polyglot:startup :verbose nil)
  (setf *readtable* (polyglot:enable-syntax (copy-readtable))))

;;; The catalog

(defvar *catalog* nil)

(defun catalog ()
  "The catalog, a list of Kotlin Book instances.  The Kotlin block calls
the catalog.kt function an ASDF component compiled: blocks compile against
the components' classes.  Its java.util.List comes back a Lisp list."
  (or *catalog* (setf *catalog* #@kt{ wonderland.catalog() })))

(defun field (object name)
  "The field NAME of OBJECT, a Kotlin data-class instance, as Lisp data.
JAVA-FIELDS reads any Java object's fields: data classes, records, Scala
case classes."
  (kotlin:to-lisp (cdr (assoc name (java-fields object) :test #'string=))))

(defun find-book (isbn)
  (find isbn (catalog) :key (lambda (book) (field book "isbn")) :test #'string=))

;;; Money

(defun round-cents (amount)
  "AMOUNT rounded to the cent, half up.  The Kotlin block's one splice has
a static type, (THE DOUBLE-FLOAT ...), so it was compiled when this file
was: the fasl holds its class, and no Kotlin compiler runs for it now."
  (declare (type double-float amount))
  #@kt{ Math.round($(the double-float amount) * 100) / 100.0 })

;;; Carroll week: a sale, added to the Kotlin catalog from Lisp.  A Kotlin
;;; method is a CLOS generic function, so Lisp can give it :before, :after
;;; and :around methods.  This :around method on Book's getPrice() takes
;;; 20% off Lewis Carroll's books wherever the price is read -- by compiled
;;; Kotlin code too -- without changing catalog.kt.
(define-java-method :around ("wonderland.Book" "getPrice()D") (book)
  (let ((price (call-next-method)))
    (if (string= (field book "author") "Lewis Carroll")
        (round-cents (* price 0.8d0))
        price)))

(defun price (book)
  "BOOK's price as the shop sells it: read through Kotlin's getter, so the
sale applies.  (FIELD reads the field itself, the list price.)"
  #@kt{ ($book as wonderland.Book).price })

;;; Validating order lines

(define-condition order-line-error (error)
  ((line :initarg :line :reader order-line-text)
   (problem :initarg :problem :reader order-line-problem))
  (:report (lambda (condition stream)
             (format stream "~A: ~S" (order-line-problem condition) (order-line-text condition)))))

(defun valid-isbn-p (isbn)
  "True when ISBN has 13 digits and a correct check digit: the digits,
weighted alternately 1 and 3, sum to a multiple of 10.  $isbn passes the
Lisp string to JScheme; its #t and #f come back as T and NIL."
  #@jscheme{ (and (= (string-length $isbn) 13)
                  (let loop ((i 0) (sum 0))
                    (if (= i 13)
                        (= 0 (remainder sum 10))
                        (loop (+ i 1)
                              (+ sum (* (if (even? i) 1 3)
                                        (- (char->integer (string-ref $isbn i)) 48))))))) })

(defun parse-order-line (text)
  "The Kotlin OrderLine for the line TEXT, \"ISBN QUANTITY\", or an
ORDER-LINE-ERROR.  Kotlin's parser throws Java exceptions, which arrive as
Lisp conditions of the matching type: a bad quantity's
NumberFormatException is a FOREIGN-NUMBER-FORMAT-ERROR, a subtype of both
FOREIGN-ILLEGAL-ARGUMENT-ERROR (its Java superclass) and PARSE-ERROR."
  (let ((line (handler-case #@kt{ wonderland.parseOrderLine($text) }
                (foreign-number-format-error ()
                  (error 'order-line-error :line text :problem "the quantity is not a number"))
                (foreign-illegal-argument-error ()
                  (error 'order-line-error :line text :problem "expected an ISBN and a quantity")))))
    (cond ((not (valid-isbn-p (field line "isbn")))
           (error 'order-line-error :line text :problem "the ISBN's check digit is wrong"))
          ((null (find-book (field line "isbn")))
           (error 'order-line-error :line text :problem "no such book"))
          (t line))))

;;; Stock

(defun substitute-for (book ordered)
  "The ISBN of the first book by BOOK's author that is in stock and not in
ORDERED (a list of ISBNs), or NIL.  The candidates go to Kawa as a list of
(isbn stock) lists; the search escapes through a continuation, call/cc, as
soon as it finds one."
  (let ((candidates (loop for other in (catalog)
                          when (and (string= (field other "author") (field book "author"))
                                    (not (member (field other "isbn") ordered :test #'string=)))
                            collect (list (field other "isbn") (field other "stock")))))
    #@kawa{ (call-with-current-continuation
              (lambda (return)
                (for-each (lambda (candidate)
                            (if (> (cadr candidate) 0) (return (car candidate))))
                          $candidates)
                #f)) }))

(defun order-line (isbn quantity)
  "A new Kotlin OrderLine."
  #@kt{ wonderland.OrderLine($isbn, $quantity) })

;;; Pricing

(defun discount (category quantity)
  "The fraction off for QUANTITY copies of a book in CATEGORY: the store's
rules, as a Scala match on a tuple, with guards.  The block compiles once
for each set of splice types it sees -- here a String and an Int -- and is
called directly after that."
  #@sc{ ($category, $quantity) match {
          case ("Computing", q) if q >= 2 => 0.15
          case ("Fiction", q) if q >= 3   => 0.10
          case (_, q) if q >= 5           => 0.05
          case _                          => 0.0
        } })

;;; The store's pricing policy, a Java class written in Lisp.  Kotlin's
;;; pricing engine takes a java.util.function.ToDoubleBiFunction; this is
;;; one, generated as a real class file, so Kotlin calls APPLY-AS-DOUBLE as
;;; it would any Java object's.  TIER is a slot of the Lisp object.
(define-java-class store-policy (:implements ("java.util.function.ToDoubleBiFunction"))
    ((tier :initform "standard"))
  ("applyAsDouble(Ljava/lang/Object;Ljava/lang/Object;)D" (self book quantity)
    (let ((quantity (kotlin:to-lisp quantity)))
      (round-cents (* (price book) quantity
                      (- 1 (discount (field book "category") quantity))
                      (if (string= tier "gold") 0.95d0 1d0))))))

(defun price-lines (lines tier)
  "Each line's price, from Kotlin's engine: it prices the lines
concurrently with coroutines, asking the Lisp policy for each.  LINES, a
Lisp list, reaches Kotlin as a List; the policy object as itself."
  (let ((policy (make-java-object 'store-policy :tier tier)))
    #@kt{ @Suppress("UNCHECKED_CAST")
          wonderland.priceLines($lines as List<wonderland.OrderLine>,
                                $policy as java.util.function.ToDoubleBiFunction<wonderland.Book, Int>) }))

;;; The invoice

(defun invoice (number customer tier rows total)
  "The invoice, rendered by a Groovy template.  ROWS, a Lisp vector of
hash tables, reaches Groovy as a java.util.List through LISP-LIST-VIEW:
not copied, each row converted to a Groovy map as the template reads it.
WordUtils, from commons-text, is on Groovy's classpath because the ASDF
system's :maven-jar component put it on every language's."
  (let ((rows (lisp-list-view rows :convert #'groovy:from-lisp)))
    ;; $x splices outside Groovy's strings; inside them, ${...} is Groovy's
    ;; own interpolation of the locals.
    #@gv{ import org.apache.commons.text.WordUtils
          def (number, customer, tier) = [$number, $customer, $tier]
          def out = new StringBuilder()
          out << 'Wonderland Books'.padRight(50) << "Invoice ${number}\n"
          out << "Bill to: ${customer} (${tier})\n\n"
          out << String.format('%-30s %4s %8s %5s %9s%n', 'Title', 'Qty', 'Each', 'Off', 'Amount')
          $rows.each { row ->
            def title = WordUtils.wrap(row.title, 30).split('\n')
            out << String.format('%-30s %4d %8.2f %4d%% %9.2f%n',
                                 title[0], row.quantity, row.price, row.off, row.amount)
            title.drop(1).each { out << "  ${it}\n" }
            if (row.note) out << "  (${row.note})\n"
          }
          out << String.format('%n%-50s %9.2f%n', 'Total', $total)
          out.toString() }))

(defun in-words (amount)
  "AMOUNT in words, for the invoice: FORMAT's ~R, run by ABCL, a Common
Lisp that runs as Java bytecode."
  #@abcl{ (multiple-value-bind (dollars cents) (floor (round (* $amount 100)) 100)
            (format nil "~@(~R~) dollar~:P and ~R cent~:P" dollars cents)) })

;;; Recommendations

(defparameter *order-history*
  '(("9780262510875" "9780262560993")
    ("9780262510875" "9780465026562" "9780262560993")
    ("9780262510875" "9780262561006")
    ("9780141439761" "9780141195346")
    ("9780141439761" "9780140434903" "9780141195346")
    ("9780465026562" "9780262510875"))
  "Past orders, as lists of ISBNs.")

(defparameter *also-bought*
  ;; A Clojure function, written in a #[ ] block.  It comes to Lisp as a
  ;; FOREIGN-FUNCTION, which FUNCALL calls like any Lisp function,
  ;; converting its arguments (Lisp lists become Clojure lists) and result.
  #[ (fn [isbn history]
       (->> history
            (filter #(some #{isbn} %))
            (mapcat identity)
            (remove #{isbn})
            frequencies
            (sort-by (juxt (comp - val) key))
            (map key))) ]
  "The ISBNs most often bought together with an ISBN, most often first.")

(defun recommendations (ordered)
  "For each ISBN in ORDERED, the in-stock books most often bought with it
that the order doesn't already have."
  (loop for isbn in ordered
        for others = (remove-if (lambda (other)
                                  (or (member other ordered :test #'string=)
                                      (zerop (field (find-book other) "stock"))))
                                (funcall *also-bought* isbn *order-history*))
        when others
          collect (cons isbn (subseq others 0 (min 2 (length others))))))

;;; The program

(defun read-order (file)
  "The order in FILE: (values customer tier item-lines)."
  (with-open-file (in file)
    (loop with customer = "unknown" and tier = "standard"
          for line = (read-line in nil)
          while line
          do (setf line (string-trim '(#\Space #\Tab) line))
          if (or (zerop (length line)) (char= (char line 0) #\#))
            do (progn)
          else if (eql 0 (search "customer " line))
            do (setf customer (subseq line 9))
          else if (eql 0 (search "tier " line))
            do (setf tier (subseq line 5))
          else collect line into items
          finally (return (values customer tier items)))))

(defun run (&optional (file (asdf:system-relative-pathname "polyglot-showcase" "order-1042.txt")))
  "Process the order in FILE and print its invoice."
  (multiple-value-bind (customer tier items) (read-order file)
    (let* ((number (or (ignore-errors (parse-integer (pathname-name file) :start 6)) 0))
           ;; Each line is parsed in a restart, SKIP-LINE; the handler
           ;; reports what was wrong and takes it, so one bad line doesn't
           ;; stop the order.
           (lines (handler-bind ((order-line-error
                                   (lambda (condition)
                                     (format t "Skipped a line: ~A~%" condition)
                                     (invoke-restart 'skip-line))))
                    (loop for text in items
                          for line = (restart-case (parse-order-line text)
                                       (skip-line () nil))
                          when line collect line)))
           (ordered (mapcar (lambda (line) (field line "isbn")) lines))
           (notes (make-hash-table :test #'equal)))
      ;; Out-of-stock books are replaced by the author's next in-stock title.
      (setf lines
            (loop for line in lines
                  for book = (find-book (field line "isbn"))
                  collect (if (plusp (field book "stock"))
                              line
                              (let ((substitute (substitute-for book ordered)))
                                (cond (substitute
                                       (setf (gethash substitute notes)
                                             (format nil "replaces ~A, out of stock" (field book "title")))
                                       (push substitute ordered)
                                       (order-line substitute (field line "quantity")))
                                      (t (format t "Skipped ~A: out of stock~%" (field book "title"))
                                         nil))))))
      (setf lines (remove nil lines))
      (let* ((prices (price-lines lines tier))
             (rows (map 'vector
                        (lambda (line price)
                          (let* ((book (find-book (field line "isbn")))
                                 (quantity (field line "quantity"))
                                 (row (make-hash-table :test #'equal)))
                            (setf (gethash "title" row) (field book "title")
                                  (gethash "quantity" row) quantity
                                  (gethash "price" row) (price book)
                                  (gethash "off" row) (round (* 100 (discount (field book "category") quantity)))
                                  (gethash "amount" row) price
                                  (gethash "note" row) (gethash (field book "isbn") notes))
                            row))
                        lines prices))
             (total (round-cents (reduce #'+ prices))))
        (format t "~%~A" (invoice number customer tier rows total))
        (format t "~A~%" (in-words total))
        (let ((suggestions (recommendations (mapcar (lambda (line) (field line "isbn")) lines))))
          (when suggestions
            (format t "~%Readers who bought these also enjoyed:~%")
            (loop for title in (remove-duplicates
                                (loop for (nil . isbns) in suggestions
                                      append (mapcar (lambda (isbn) (field (find-book isbn) "title")) isbns))
                                :test #'string= :from-end t)
                  do (format t "  ~A~%" title))))
        total))))
