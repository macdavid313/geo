;;;; wkt.cl
(in-package #:wkt)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------ token ------------------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype token ()
  "Phony type definition for token."
  '(or real
    (member LPAREN RPAREN COMMA
     EMPTY POINT LINESTRING POLYGON
     MULTIPOINT MULTILINESTRING MULTIPOLYGON GEOMETRYCOLLECTION
     ZM Z M)))

(defconstant +keywords+
  (list 'LPAREN 'RPAREN 'COMMA
        'EMPTY 'POINT 'LINESTRING 'POLYGON
        'MULTIPOINT 'MULTILINESTRING 'MULTIPOLYGON 'GEOMETRYCOLLECTION
        'ZM 'Z 'M))

(defmacro token-type (x)
  `(if (symbolp ,x) :keyword :number))

(define-condition wkt-lex-error (error)
  ((line :initarg :line :type fixnum)
   (column :initarg :column :type fixnum))
  (:report (lambda (c stream)
             (with-slots (line column) c
               (format stream "WKT lex error at line: ~d, column: ~d" line column)))))

(ff:def-foreign-call strtod ((str :foreign-address)
                             (endptr :foreign-address))
  :returning :double)

(defun lex (s k)
  (declare (type string s)                       ; WKT string
           (ftype (function (list) geometry) k)) ; a continuation that handles the tokens
  (with-stack-list (tokens)
    (with-underlying-simple-vector (s s start len)
      (let ((c #\0)
            (i start)
            (line 1)
            (column 1)
            (offset 0))
        (declare (type character c)
                 (type fixnum len line column offset)
                 (dynamic-extent c i len line column offset))
        (while (< i len)
          (setq c (schar s i))
          (cond ((or (char= c #\Newline) (char= c #\Return)) ; new line
                 (incf i)
                 (incf line)
                 (setq column 0))

                ((or (char= c #\Space) (char= c #\Tab)) ; space or tab
                 (incf i)
                 (incf column))

                ((or (char= c #\() (char= c #\)) (char= c #\,)) ; lparen, rparen, or comma
                 (push (case c
                         (#\( 'LPAREN)
                         (#\) 'RPAREN)
                         (#\, 'COMMA))
                       tokens)
                 (incf i)
                 (incf column))

                ((or (digit-char-p c) (char= c #\+) (char= c #\-) (char= c #\.)) ; +, -, ., or digits
                 (multiple-value-bind (match-p submatch)
                     (match-re "[+-]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?"
                               s
                               :start i
                               :ignore-whitespace nil
                               :return :index)
                   (unless match-p
                     (error 'wkt-lex-error :line line :column column))
                   (setq offset (- (cdr submatch) (car submatch)))
                   (push (with-native-string (str s :start i :end (+ i offset))
                           (strtod str 0))
                         tokens)
                   (incf i offset)
                   (incf column offset)))

                ((alpha-char-p c)       ; keywords
                 (setq offset 1)
                 (while (and (< (+ i offset) len)
                             (alpha-char-p (schar s (+ i offset))))
                   (incf offset))
                 (multiple-value-bind (token status)
                     (intern (string-upcase (make-array offset
                                                        :element-type 'character
                                                        :displaced-to s
                                                        :displaced-index-offset i))
                             'geo.wkt)
                   (declare (ignore status))
                   (unless (member token +keywords+)
                     (error 'wkt-lex-error :line line :column column))
                   (push token tokens))
                 (incf i offset)
                 (incf column offset))

                (t (error 'wkt-lex-error :line line :column column))))
        (funcall k (nreverse tokens))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -------------------------- token cursor -------------------------- ;;
;; ------------------ simulating a stream of token ------------------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct token-cursor
  (tokens nil :type list)
  (len 0 :type fixnum)
  (ptr 0 :type fixnum))

(defun token-cursor-empty-p (s)
  (= (the fixnum (token-cursor-len s))
     (the fixnum (token-cursor-ptr s))))

(defun consume (s)
  "Move ptr forward, unless the stream is empty."
  (unless (token-cursor-empty-p s)
    (setf (token-cursor-tokens s) (cdr (token-cursor-tokens s)))
    (incf (token-cursor-ptr s))))

(defun peek-token (s)
  "Get the current token without moving forward the ptr; returns nil if stream is
empty."
  (unless (token-cursor-empty-p s)
    (car (token-cursor-tokens s))))

(define-condition wkt-parser-unexpected-token-error (error)
  ((token :initarg :token :type token))
  (:report (lambda (c stream)
             (format stream "WKT Parsing error: unexpected token ~a"
                     (slot-value c 'token)))))

(define-condition wkt-parser-unexpected-eof-error (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "WKT Parsing error: unexpected EOF"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------ token stream utilities ------------------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unexpected-eof-error-maybe (s)
  "Signal 'wkt-parser-unexpected-eof-error if the stream is empty."
  (when (token-cursor-empty-p s)
    (error 'wkt-parser-unexpected-eof-error)))

(defun expect-eof (s)
  "Signal 'wkt-parser-unexpected-token-error unless the stream if empty."
  (unless (token-cursor-empty-p s)
    (error 'wkt-parser-unexpected-token-error :token (peek-token s))))

(defun expect-keyword (s expected-value)
  (unexpected-eof-error-maybe s)
  (unless (eq (peek-token s) expected-value)
    (error 'wkt-parser-unexpected-token-error :token (peek-token s)))
  (consume s))

(defun expect-keyword-maybe (s expected-value)
  (unless (token-cursor-empty-p s)
    (when (eq (peek-token s) expected-value)
      (consume s)
      t)))

(defun expect-number (s)
  (unexpected-eof-error-maybe s)
  (let ((token (peek-token s)))
    (unless (eq (token-type (peek-token s)) :number)
      (error 'wkt-parser-unexpected-token-error :token token))
    (consume s)
    token))

(defun expect-number-maybe (s)
  (unless (token-cursor-empty-p s)
    (let (token)
      (setq token (peek-token s))
      (when (eq (token-type token) :number)
        (consume s)
        token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------------ Parser Implementation ------------------------------ ;;
;; ----- WKT BNF: https://github.com/postgis/postgis/blob/master/doc/bnf-wkt.txt ----- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-wkt-representation (s)
  (unexpected-eof-error-maybe s)
  (cond ((expect-keyword-maybe s 'POINT)
         (parse-point-text s))
        ((expect-keyword-maybe s 'LINESTRING)
         (parse-line-string s 'line-string))
        ((expect-keyword-maybe s 'POLYGON)
         (parse-polygon s))
        ((expect-keyword-maybe s 'MULTIPOINT)
         (parse-multi-point s))
        ((expect-keyword-maybe s 'MULTILINESTRING)
         (parse-multi-line-string s))
        ((expect-keyword-maybe s 'MULTIPOLYGON)
         (parse-multi-polygon s))
        ((expect-keyword-maybe s 'GEOMETRYCOLLECTION)
         (parse-geometry-collection s))
        (t (error 'wkt-parser-unexpected-token-error :token (peek-token s)))))

(defun parse-point-text (s)
  (parse-zm-maybe s)
  (unexpected-eof-error-maybe s)
  (if* (eq (peek-token s) 'EMPTY)
     then (consume s)
          +empty-point+
     else (expect-keyword s 'LPAREN)
          (let ((coord (parse-point s)))
            (expect-keyword s 'RPAREN)
            (make-point-from-coord coord))))

(defun parse-point (s)
  (let (x y z)
    (setq x (expect-number s)           ; x
          y (expect-number s)           ; y
          z (expect-number-maybe s))    ; z
    (expect-number-maybe s)             ; m
    (make-coord x y z)))

(defun parse-line-string (s output-type)
  (parse-zm-maybe s)
  (unexpected-eof-error-maybe s)
  (if* (eq (peek-token s) 'EMPTY)
     then (consume s)
          (if (eq output-type 'line-string) +empty-line-string+ +empty-linear-ring+)
     else (expect-keyword s 'LPAREN)
          (with-stack-list (coords (parse-point s))
            (while (expect-keyword-maybe s 'COMMA)
              (push (parse-point s) coords))
            (expect-keyword s 'RPAREN)
            (if* (eq output-type 'line-string)
               then (make-line-string-from-coords (nreverse coords))
               else (make-linear-ring-from-coords (nreverse coords))))))

(defun parse-polygon (s)
  (parse-zm-maybe s)
  (unexpected-eof-error-maybe s)
  (if* (eq (peek-token s) 'EMPTY)
     then (consume s)
          +empty-polygon+
     else (expect-keyword s 'LPAREN)
          (with-stack-list (rings (parse-line-string s 'linear-ring))
            (while (expect-keyword-maybe s 'COMMA)
              (push (parse-line-string s 'linear-ring) rings))
            (expect-keyword s 'RPAREN)
            (make-polygon-from-linear-rings (nreverse rings)))))

(defun parse-multi-point (s)
  (parse-zm-maybe s)
  (unexpected-eof-error-maybe s)
  (if* (eq (peek-token s) 'EMPTY)
     then (consume s)
          +empty-multi-point+
     else (expect-keyword s 'LPAREN)
          (with-stack-list (items (make-point-from-coord (parse-point s)))
            (while (expect-keyword-maybe s 'COMMA)
              (push (make-point-from-coord (parse-point s)) items))
            (expect-keyword s 'RPAREN)
            (make-multi-point (nreverse items)))))

(defun parse-multi-line-string (s)
  (parse-zm-maybe s)
  (unexpected-eof-error-maybe s)
  (if* (eq (peek-token s) 'EMPTY)
     then (consume s)
          +empty-multi-line-string+
     else (expect-keyword s 'LPAREN)
          (with-stack-list (items (parse-line-string s 'line-string))
            (while (expect-keyword-maybe s 'COMMA)
              (push (parse-line-string s 'line-string) items))
            (expect-keyword s 'RPAREN)
            (make-multi-line-string (nreverse items)))))

(defun parse-multi-polygon (s)
  (parse-zm-maybe s)
  (unexpected-eof-error-maybe s)
  (if* (eq (peek-token s) 'EMPTY)
     then (consume s)
          +empty-multi-polygon+
     else (expect-keyword s 'LPAREN)
          (with-stack-list (items (parse-polygon s))
            (while (expect-keyword-maybe s 'COMMA)
              (push (parse-polygon s) items))
            (expect-keyword s 'RPAREN)
            (make-multi-polygon (nreverse items)))))

(defun parse-geometry-collection (s)
  (parse-zm-maybe s)
  (unexpected-eof-error-maybe s)
  (if* (eq (peek-token s) 'EMPTY)
     then (consume s)
          +empty-geometry-collection+
     else (expect-keyword s 'LPAREN)
          (with-stack-list (items (parse-wkt-representation s))
            (while (expect-keyword-maybe s 'COMMA)
              (push (parse-wkt-representation s) items))
            (expect-keyword s 'RPAREN)
            (make-geometry-collection (nreverse items)))))

(defun parse-zm-maybe (s)
  (unexpected-eof-error-maybe s)
  (when (member (peek-token s) '(Z M ZM) :test 'eq)
    (consume s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------- API ----------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (ftype (function (list) geometry) parse/k))
(defun parse/k (tokens)
  (let* ((token-cursor (make-token-cursor :tokens tokens
                                          :len (length tokens)
                                          :ptr 0))
         (g (parse-wkt-representation token-cursor)))
    (declare (dynamic-extent token-cursor))
    (expect-eof token-cursor)
    g))

(defun read-wkt (s)
  (check-type s string)
  (lex s #'parse/k))

(define-compiler-macro read-wkt (&whole form &environment env s)
  (if* (and (constantp s env) (stringp s))
     then (lex s #'parse/k)
     else form))

(defparameter *wkt-writer-rounding-precision* 16)

(defparameter *wkt-writer-trim-p* nil)

(defun write-wkt (g &optional stream)
  (labels ((float-formatter (f)
             (check-type *wkt-writer-rounding-precision* (integer 0 *))
             (let (str)
               (setf str (format stream (string+ "~," *wkt-writer-rounding-precision* "f") f))
               (when *wkt-writer-trim-p*
                 (setq str (string-right-trim '#.(list #\0) str)))
               (when (= (position #\. str :test 'char= :from-end t)
                        #1=(1- (length str)))
                 (setq str (make-array #1# :element-type 'character
                                           :displaced-to str
                                           :displaced-index-offset 0)))
               str))

           (write-xyz (x y z stream)
             (declare (type double-float x y)
                      (type (or null double-float) z))
             (write-string (float-formatter x) stream)
             (write-char #\Space stream)
             (write-string (float-formatter y) stream)
             (when z
               (write-char #\Space stream)
               (write-string (float-formatter z) stream)))

           (write-point (g stream)
             (declare (type point g))
             (multiple-value-call #'write-xyz (point-xyz g) stream))

           (write-line-string (g stream)
             (declare (type (or line-string linear-ring) g))
             (write-char #\( stream)
             (do* ((coords (coordinates g))
                   (ncoords (length coords))
                   (i 0 (1+ i)))
                  ((= i ncoords))
               (multiple-value-call #'write-xyz (coord-xyz (aref coords i)) stream)
               (unless (= i (1- ncoords))
                 (write-string ", " stream)))
             (write-char #\) stream))

           (write-polygon (g stream)
             (declare (type polygon g))
             (write-char #\( stream)
             (let* ((exterior (polygon-exterior g))
                    (interiors (polygon-interiors g))
                    (ninteriors (length interiors)))
               (write-line-string exterior stream)
               (do ((i 0 (1+ i)))
                   ((= i ninteriors))
                 (write-string ", " stream)
                 (write-line-string (aref interiors i) stream)))
             (write-char #\) stream))

           (write-g-items (g writer stream)
             (write-char #\( stream)
             (do* ((items (coordinates g))
                   (nitems (length items))
                   (i 0 (1+ i)))
                  ((= i nitems))
               (funcall writer (aref items i) stream)
               (unless (= i (1- nitems))
                 (write-string ", " stream)))
             (write-char #\) stream))

           (write-g (g stream)
             (let ((name (typecase g
                           (point "POINT")
                           (line-string "LINESTRING")
                           (linear-ring "LINEARRING")
                           (polygon "POLYGON")
                           (rect "RECT")
                           (multi-point "MULTIPOINT")
                           (multi-line-string "MULTILINESTRING")
                           (multi-polygon "MULTIPOLYGON")
                           (geometry-collection "GEOMETRYCOLLECTION")))
                   (dim (dim g)))
               (declare (type simple-string name)
                        (type (integer 2 3) dim))
               (if* (geometry-empty-p g)
                  then (write-string (string+ name #\Space "EMPTY") stream)
                  else (write-string name stream)
                       (when (= dim 3) (write-string " Z " stream))
                       (typecase g
                         ((or point line-string linear-ring)
                          (write-line-string g stream))
                         (polygon
                          (write-polygon g stream))
                         (rect (error "No WKT writer implemented for RECT"))
                         (multi-point
                          (write-g-items g #'write-point stream))
                         (multi-line-string
                          (write-g-items g #'write-line-string stream))
                         (multi-polygon
                          (write-g-items g #'write-polygon stream))
                         (geometry-collection
                          (write-g-items g #'write-g stream)))))))

    (declare (dynamic-extent #'float-formatter
                             #'write-xyz
                             #'write-point
                             #'write-line-string
                             #'write-polygon
                             #'write-g-items
                             #'write-g))

    (check-type g geometry)
    (check-type stream (or null (satisfies output-stream-p)))

    (if* stream
       then (write-g g stream)
       else (with-output-to-string (o)
              (write-g g o)))))
