(in-package #:pdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun build-glyph-list (file)
  (let ((ht (make-hash-table :test 'equal :size 4281))
	(*read-base* 16))
    (with-open-file (f file)
      (loop for line = (read-line f nil)
	    while line
	    unless (char= #\# (char line 0))
	      do (let* ((semi (position #\; line))
			(name (subseq line 0 semi))
			(code (read-from-string (subseq line (1+ semi)))))
		   (setf (gethash name ht) code))))
    ht)))

(defvar +adobe-glyph-list+ (build-glyph-list (asdf:system-relative-pathname "cl-pdf-parser" "glyphlist.txt")))

(defun get-page (document page-number)
  (aref (pages (root-page document)) page-number))

(defun get-page-content (page)
  (content (content (get-dict-value (content page) "/Contents"))))

(defun get-page-resources (page)
  (content (get-dict-value (content page) "/Resources")))

(defun get-page-font (page font-name)
  (content (get-dict-value (get-dict-value (get-page-resources page) "/Font") font-name)))

(defun get-font-encoding (font)
  (let ((entry (get-dict-value font "/Encoding")))
    (typecase entry
      (string
       (get-encoding (subseq entry 1)))
      (indirect-object
       (assert (string= "/Encoding" (get-dict-value (content entry) "/Type")))
       (let ((differences (get-dict-value (content entry) "/Differences")))
	 (get-encoding "StandardEncoding"))))))

(defun get-character-name (font char-code)
  (aref (char-names (get-font-encoding font)) char-code))

(defun character-code->unicode-value (font character-code)
  (gethash (get-character-name font (char-code character-code)) +adobe-glyph-list+))

(defun get-character-width (font character-code)
  (let ((first (get-dict-value font "/FirstChar"))
	(last (get-dict-value font "/LastChar")))
    (if (<= first character-code last)
	(aref (get-dict-value font "/Widths") (- character-code first))
	(get-missing-width font))))

(defun get-space-width (font)
  (let ((char-width (get-character-width font #.(char-code #\Space))))
    (if (zerop char-width)
	(let ((widths (get-dict-value font "/Widths")))
	  (loop for w across widths
		unless (zerop w)
		  sum w into sum count w into count
		finally (return (float (/ sum count)))))
	char-width)))

(defun get-missing-width (font)
  (or (get-dict-value (content (get-dict-value font "/FontDescriptor")) "/MissingWidth")
      0))	  

(defun get-page-external-graphics-state (page external-graphics-state-name)
  (content (get-dict-value (get-dict-value (get-page-resources page) "/ExtGState") external-graphics-state-name)))

(defun read-page-content (page)
  (let ((*contentp* t)
	(page-content (get-page-content page))
	(result '()))
    (dolist (c page-content (nreverse result))
      (with-input-from-string (*pdf-input-stream* c)
	(loop for obj = (read-object nil)
	      until (eql :eof obj)
	      do (push obj result))))))

(defun group-operators (content-list)
  (loop with operands = '()
	for obj in content-list
	if (typep obj 'operator)
	  collect (cons obj (nreverse (copy-list operands)))
	  and do (setf operands '())
	else do (push obj operands)))
	  
(defclass graphics-state ()
  ((%ctm
    :initarg :ctm
    :accessor ctm) ; initform matrix transforming default user coords to device coords
   (%clipping-path
    :initarg :clipping-path
    :accessor clipping-path) ; initform boundary of entire portion of imageable page
   (%stroking-color-space
    :accessor stroking-color-space
    :initform '|/DeviceGray|)
   (%nonstroking-color-space
    :accessor nonstroking-color-space
    :initform '|/DeviceGray|)
   (%stroking-color
    :accessor stroking-color
    :initform :black)
   (%nonstroking-color
    :accessor nonstroking-color
    :initform :black)
   (%line-width
    :accessor line-width
    :initform 1.0)
   (%line-cap
    :accessor line-cap
    :initform 0) ; square
   (%line-join
    :accessor line-join
    :initform 0) ; mitered
   (%miter-limit
    :accessor miter-limit
    :initform 10.0)
   (%dash-pattern
    :accessor dash-pattern
    :initform (cons #() 0)) ; FIXME
   (%rendering-intent
    :accessor rendering-intent
    :initform '|/RelativeColorimetric|)
   (%stroke-adjustment
    :accessor stroke-adjustment
    :initform :false) ; NIL ?
   (%blend-mode
    :accessor blend-mode
    :initform '|/Normal|)
   (%soft-mask
    :accessor soft-mask
    :initform '|/None|)
   (%stroking-alpha-constant
    :accessor stroking-alpha-constant
    :initform 1.0)
   (%nonstroking-alpha-constant
    :accessor nonstroking-alpha-constant
    :initform 1.0)
   (%alpha-source
    :accessor alpha-source
    :initform :false) ; NIL ?
   ;; Device-Dependent Graphics State Parameters
   (%stroking-overprint
    :accessor stroking-overprint
    :initform :false) ; NIL ?
   (%nonstroking-overprint
    :accessor nonstroking-overprint
    :initform :false)
   (%overprint-mode
    :accessor overprint-mode
    :initform 0)
   (%black-generation
    :accessor black-generation)
   (%undercolor-removal
    :accessor undercolor-removal)
   (%transfer
    :accessor transfer)
   (%halftone
    :accessor halftone)
   (%flatness
    :accessor flatness
    :initform 1.0)
   (%smoothness
    :accessor smoothness)
   ;; Text state parameters
   (%character-spacing
    :accessor character-spacing
    :initform 0)
   (%word-spacing
    :accessor word-spacing
    :initform 0)
   (%horizontal-scaling
    :accessor horizontal-scaling
    :initform 1.0) ; i.e. 100%
   (%leading
    :accessor leading
    :initform 0)
   (%text-font
    :accessor text-font)
   (%text-font-size
    :accessor text-font-size)
   (%text-rendering-mode
    :accessor text-rendering-mode
    :initform 0)
   (%text-rise
    :accessor text-rise
    :initform 0)
   (%text-knockout
    :accessor text-knockout)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *content-stream-operator-table*
  '(("b" "Close, fill, and stroke path using nonzero winding number rule" 60 0 () op-close-fill-stroke)
    ("B" "Fill and stroke path using nonzero winding number rule" 60 0 () op-fill-stroke)
    ("b*" "Close, fill, and stroke path using even-odd rule" 60 0 () op-close-eo-fill-stroke)
    ("B*" "Fill and stroke path using even-odd rule" 60 0 () op-eo-fill-stroke)
    ("BDC" "Begin marked-content sequence with property list" 320 2 (name properties) op-begin-marked-content)
    ("BI" "Begin inline image object" 92 0 () op-begin-image)
    ("BMC" "Begin marked-content sequence" 320 1 (name) op-begin-marked-content)
    ("BT" "Begin text object" 107 0 () op-begin-text)
    ("BX" "Begin compatibility section" 32 0 () op-begin-ignore-undef) ; FIXME)
    ("c" "Append curved segment to path (three control points)" 59 6 (number number number number number number) op-curve-to)
    ("cm" "Concatenate matrix to current transformation matrix" 57 6 (number number number number number number) op-concat)
    ("CS" "Set color space for stroking operations" 74 1 (name) op-set-stroke-color-space)
    ("cs" "Set color space for nonstroking operations" 74 1 (name) op-set-fill-color-space)
    ("d" "Set line dash pattern" 57 2 (array number) op-set-dash)
    ("d0" "Set glyph width in Type 3 font" 113 2 (number number) op-set-char-width)
    ("d1" "Set glyph width and bounding box in Type 3 font" 113 6 (number number number number number number) op-set-cache-device)
    ("Do" "Invoke named XObject" 87 1 (name) op-x-object)
    ("DP" "Define marked-content point with property list" 320 2 (name properties) op-mark-point)
    ("EI" "End inline image object" 92 0 () op-end-image)
    ("EMC" "End marked-content sequence" 320 0 () op-end-marked-content)
    ("ET" "End text object" 107 0 () op-end-text)
    ("EX" "End compatibility section" 32 0 () op-end-ignore-undef) ; FIXME
    ("f" "Fill path using nonzero winding number rule" 60 0 () op-fill)
    ("F" "Fill path using nonzero winding number rule (obsolete)" 60 0 () op-fill)
    ("f*" "Fill path using even-odd rule" 60 0 () op-eo-fill)
    ("G" "Set gray level for stroking operations" 74 1 (number) op-set-stroke-gray)
    ("g" "Set gray level for nonstroking operations" 74 1 (number) op-set-fill-gray)
    ("gs" "Set parameters from graphics state parameter dictionary" 57 1 (name) op-set-ext-g-state) ; FIXME
    ("h" "Close subpath" 59 0 () op-close-path)
    ("i" "Set flatness tolerance" 57 1 (number) op-set-flat)
    ("ID" "Begin inline image data" 92 0 () op-image-data)
    ("j" "Set line join style" 57 1 (integer) op-set-line-join)
    ("J" "Set line cap style" 57 1 (integer) op-set-line-cap)
    ("K" "Set CMYK color for stroking operations" 74 4 (number number number number) op-set-stroke-cymk-color)
    ("k" "Set CMYK color for nonstroking operations" 74 4 (number number number number) op-set-fill-cymk-color)
    ("l" "Append straight line segment to path" 59 2 (number number) op-line-to)
    ("m" "Begin new subpath" 59 2 (number number) op-move-to)
    ("M" "Set miter limit" 57 1 (number) op-set-miter-limit)
    ("MP" "Define marked-content point" 320 1 (name) op-mark-point)
    ("n" "End path without filling or stroking" 60 0 () op-end-path)
    ("q" "Save graphics state" 57 0 () op-save)
    ("Q" "Restore graphics state" 57 0 () op-restore)
    ("re" "Append rectangle to path" 59 4 (number number number number) op-rectangle)
    ("RG" "Set RGB color for stroking operations" 74 3 (number number number) op-set-stroke-rgb-color)
    ("rg" "Set RGB color for nonstroking operations" 74 3 (number number number) op-set-fill-rgb-color)
    ("ri" "Set color rendering intent" 57 1 (name) op-set-rendering-intent)
    ("s" "Close and stroke path" 60 0 () op-close-stroke)
    ("S" "Stroke path" 60 0 () op-stroke)
    ("SC" "Set color for stroking operations" 74 nil nil op-set-stroke-color) ; 33 number
    ("sc" "Set color for nonstroking operations" 74 nil nil op-set-fill-color) ; 33 number
    ("SCN" "Set color for stroking operations (ICCBased and special colour spaces)" 74 nil nil op-set-stroke-color-n) ; 33 number
    ("scn" "Set color for nonstroking operations (ICCBased and special colour spaces)" 74 nil nil op-set-fill-color-n) ; 33 number
    ("sh" "Paint area defined by shading pattern" 77 1 (name) op-sh-fill)
    ("T*" "Move to start of next text line" 108 0 () op-text-next-line)
    ("Tc" "Set character spacing" nil 1 (number) op-set-char-spacing)
    ("Td" "Move text position" 108 2 (number number) op-text-move)
    ("TD" "Move text position and set leading" 108 2 (number number) op-text-move-set)
    ("Tf" "Set text font and size" nil 2 (name number) op-set-font)
    ("Tj" "Show text" 109 1 (string) op-show-text)
    ("TJ" "Show text, allowing individual glyph positioning" 109 1 (array) op-show-space-text)
    ("TL" "Set text leading" nil 1 (number) op-set-text-leading)
    ("Tm" "Set text matrix and text line matrix" 108 6 (number number number number number number) op-set-text-matrix)
    ("Tr" "Set text rendering mode" nil 1 (integer) op-set-text-render)
    ("Ts" "Set text rise" nil 1 (number) op-set-text-rise)
    ("Tw" "Set word spacing" nil 1 (number) op-set-word-spacing)
    ("Tz" "Set horizontal text scaling" nil 1 (number) op-set-horizontal-scaling)
    ("v" "Append curved segment to path (initial point replicated)" 59 4 (number number number number) op-curve-to-1)
    ("w" "Set line width" 57 1 (number) op-set-line-width)
    ("W" "Set clipping path using nonzero winding number rule" 61 0 () op-clip)
    ("W*" "Set clipping path using even-odd rule" 61 0 () op-eo-clip)
    ("y" "Append curved segment to path (final point replicated)" 59 4 (number number number number) op-curve-to-2)
    ("'" "Move to next line and show text" 109 1 (string) op-move-show-text)
    ("\"" "Set word and character spacing, move to next line, and show text" 109 3 (number number string) op-move-set-show-text))))

(defclass operator ()
  ((%name
    :initarg :name
    :reader operator-name)
   (%arg-count
    :initarg :arg-count
    :reader arg-count)
   (%arg-types
    :initarg :arg-types
    :reader arg-types)
   (%function
    :initarg :function
    :reader operator-function)))

(defun name-symbol (symbol)
  (and (string= "PDF" (package-name (symbol-package symbol)))
       (char= #\/ (char (symbol-name symbol) 0))))

(deftype name ()
  '(and symbol
    (satisfies name-symbol)))

;;; FIXME properties type

;; FIXME sc SC scn SCN
(defgeneric check-operands (operator operands)
  (:method ((operator operator) operands)
    (unless (= (arg-count operator) (length operands))
      (error "Wrong number of operands for ~A. Wanted ~D, got ~D."
	     (operator-name operator) (arg-count operator) (length operands)))
    (loop for operand in operands
	  for type in (arg-types operator)
	  unless (typep operand type)
	    do (error "Wrong operand type for ~A. Wanted ~A, got ~A."
		      (operator-name operator) type operand))))

(defparameter *content-stream-operators*
  (let ((ht (make-hash-table :test 'equal :size 73)))
    (dolist (entry *content-stream-operator-table* ht)
      (destructuring-bind (name comment table count types function) entry
	(declare (ignore comment table))
	(setf (gethash name ht)
	      (make-instance 'operator :name name :arg-count count :arg-types types :function function))))))

(defmethod print-object ((object operator) stream)
  (if *print-pretty*
      (princ (operator-name object) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(princ (operator-name object) stream))))

(defclass unknown-operator (operator)
  ()
  (:default-initargs :function (lambda () (error "Unknown operator")))) ; FIXME

(defclass output-device ()
  ())

(defgeneric op-close-fill-stroke (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-fill-stroke (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-close-eo-fill-stroke (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-eo-fill-stroke (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-begin-marked-content (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-begin-image (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-begin-text (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-begin-ignore-undef (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-curve-to (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-concat (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-stroke-color-space (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-fill-color-space (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-dash (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-char-width (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-cache-device (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-x-object (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-mark-point (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-end-image (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-end-marked-content (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-end-text (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-end-ignore-undef (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-fill (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-eo-fill (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-stroke-gray (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-fill-gray (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-ext-g-state (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-close-path (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-flat (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-image-data (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-line-join (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-line-cap (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-stroke-cymk-color (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-fill-cymk-color (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-line-to (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-move-to (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-miter-limit (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-mark-point (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-end-path (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-save (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-restore (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-rectangle (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-stroke-rgb-color (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-fill-rgb-color (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-rendering-intent (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-close-stroke (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-stroke (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-stroke-color (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-fill-color (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-stroke-color-n (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-fill-color-n (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-sh-fill (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-text-next-line (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-char-spacing (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-text-move (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-text-move-set (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-font (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-show-text (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-show-space-text (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-text-leading (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-text-matrix (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-text-render (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-text-rise (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-word-spacing (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-horizontal-scaling (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-curve-to-1 (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-set-line-width (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-clip (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-eo-clip (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-curve-to-2 (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-move-show-text (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))

(defgeneric op-move-set-show-text (operands graphics-state output-device)
  (:method (operands graphics-state output-device)
    nil))


(defgeneric render-page (page output-device))

;;;; Text Output / Extraction Rendering

(defun unescape-literal-string (string)
  (with-output-to-string (out)
	(loop with escape = nil
	      with octal = 0
	      with octal-count = 0
	      for char across (subseq string 1 (1- (length string)))
	      if (char= #\\ char)
		do (cond (escape
			  (write-char #\\ out)
			  (setf escape nil))
			 (t
			  (setf escape t)))
	      else if (and (not (zerop octal))
			   (digit-char-p char 8))
		     do (cond ((= octal-count 2)
			       (write-char (code-char (logand (+ (* octal 8) (digit-char-p char 8)) #o377)) out)
			       (setf octal 0
				     octal-count 0))
			      (t
			       (setf octal (+ (* octal 8) (digit-char-p char 8)))
			       (incf octal-count)))
	      else do (cond (escape
			     (case char
			       (#\Newline
				nil)
			       (#\n
				(write-char #\LF out))
			       (#\r
				(write-char #\CR out))
			       (#\t
				(write-char #\HT out))
			       (#\b
				(write-char #\BS out))
			       (#\f
				(write-char #\FF out))
			       (#\(
				(write-char #\( out))
			       (#\)
				(write-char #\) out))
			       (t
				(if (digit-char-p char 8)
				    (setf octal (digit-char-p char 8)
					  octal-count 1)
				    (write-char char out))))
			     (setf escape nil))
			    (t
			     (when (not (zerop octal))
			       (write-char (code-char (logand octal #o377)) out)
			       (setf octal 0
				     octal-count 0))
			     (write-char char out)))
	      finally (when (not (zerop octal))
			(write-char (code-char (logand octal #o377)) out)))
			    
      string))

(defun unescape-hexadecimal-string (string)
  (with-output-to-string (out)
    (loop with prev = nil
	  for char across (subseq string 1 (1- (length string)))
	  unless (white-char-p char)
	    do (cond (prev
		      (write-char (code-char (+ (* prev 16) (digit-char-p char 16))) out)
		      (setf prev nil))
		     (t
		      (setf prev (digit-char-p char 16))))
	  finally (when prev
		  (write-char (code-char (* prev 16)) out)))))

(defun unescape-string (string)
  (cond ((and (>= (length string) 2)
	      (char= #\( (char string 0)))
	 (unescape-literal-string string))
	((and (>= (length string) 2)
	      (char= #\< (char string 0)))
	 (unescape-hexadecimal-string string))
	(t
	 (error "Unknown string format: ~S" string))))

(defclass text-output-device (output-device)
  ((%output
    :accessor text-output
    :initform '())
   (%gs-stack
    :initform '()
    :accessor gs-stack)
   (%text-matrix
    :accessor text-matrix)
   (%prev-tm
    :accessor previous-matrix)
   (%text-line-matrix ; ???
    :accessor text-line-matrix)
   (%space-width
    :initform +default-space-width+
    :accessor space-width)
   (%cmaps
    :initform '()
    :accessor cmaps)))

(defun get-unicode-char (page device font-name char-code)
  (let ((encoding (cdr (assoc font-name (cmaps device)))))
    (if encoding
	(aref encoding char-code)
	(let ((encoding (get-font-encoding (get-page-font page font-name))))
	  (push (cons font-name encoding) (cmaps device))
	  (aref encoding char-code)))))

;;; Text matrices

(defvar +identity-text-matrix+ #(1 0 0 0 1 0 0 0 1))

(defun make-matrix (&optional (initial-contents +identity-text-matrix+))
  (make-array 9 :initial-contents initial-contents))

(defun make-translation-matrix (tx ty)
  (make-array 9 :initial-contents (list 1 0 0 0 1 0 tx ty 1)))

(defun m* (m n)
  "Multiply two 3 x 3 matrices represented as 9 length vectors"
  (assert (= 9 (length m) (length n)))
  (vector (+ (* (svref m 0) (svref n 0))
	     (* (svref m 1) (svref n 3))
	     (* (svref m 2) (svref n 6)))
	  (+ (* (svref m 0) (svref n 1))
	     (* (svref m 1) (svref n 4))
	     (* (svref m 2) (svref n 7)))
	  (+ (* (svref m 0) (svref n 2))
	     (* (svref m 1) (svref n 5))
	     (* (svref m 2) (svref n 8)))
	  (+ (* (svref m 3) (svref n 0))
	     (* (svref m 4) (svref n 3))
	     (* (svref m 5) (svref n 6)))
	  (+ (* (svref m 3) (svref n 1))
	     (* (svref m 4) (svref n 4))
	     (* (svref m 5) (svref n 7)))
	  (+ (* (svref m 3) (svref n 2))
	     (* (svref m 4) (svref n 5))
	     (* (svref m 5) (svref n 8)))
	  (+ (* (svref m 6) (svref n 0))
	     (* (svref m 7) (svref n 3))
	     (* (svref m 8) (svref n 6)))
	  (+ (* (svref m 6) (svref n 1))
	     (* (svref m 7) (svref n 4))
	     (* (svref m 8) (svref n 7)))
	  (+ (* (svref m 6) (svref n 2))
	     (* (svref m 7) (svref n 5))
	     (* (svref m 8) (svref n 8)))))

;;; methods on text-output-device

;; Tc
(defmethod op-set-char-spacing (operands gs (device text-output-device))
  (setf (character-spacing gs) (first operands)))

;; Tw
(defmethod op-set-word-spacing (operands gs (device text-output-device)) ; FIXME general?
  (setf (word-spacing gs) (first operands))) ; FIXME 1.0 + ?

;; Tz
(defmethod op-set-horizontal-scaling (operands gs (device text-output-device)) ; FIXME general?
  (setf (horizontal-scaling gs) (/ (first operands) 100.0)))

;; TL
(defmethod op-set-text-leading (operands gs (device text-output-device)) ; FIXME general?
  (setf (leading gs) (first operands)))

(defconstant +default-space-width+ 400)

;; Tf
(defmethod op-set-font (operands gs (device text-output-device))
  (let* ((font (get-page-font *page* (first operands)))
	 (space-width (get-space-width font)))
    (setf (text-font gs) font
	  (text-font-size gs) (second operands)
	  (space-width device) space-width)))

;; Tr
(defmethod op-set-text-render (operands gs (device text-output-device))
  (setf (text-rendering-mode gs) (first operands)))

;; Ts
(defmethod op-set-text-rise (operands gs (device text-output-device))
  (setf (text-rise gs) (first operands)))

;; BT
(defmethod op-begin-text (operands gs (device text-output-device))
  (setf (text-matrix device) (make-matrix)
	(text-line-matrix device) (make-matrix)
	(previous-matrix device) (make-matrix)))

;; ET
(defmethod op-end-text (operands gs (device text-output-device))
  (setf (text-matrix device) nil
	(text-line-matrix device) nil
	(previous-matrix device) nil))

;; Td
(defmethod op-text-move (operands gs (device text-output-device))
  (let* ((tx (first operands))
	 (ty (second operands))
	 (translation (make-translation-matrix tx ty))
	 (new-tm (m* (text-matrix device) translation)))
    (setf (text-matrix device) new-tm
	  (text-line-matrix device) new-tm)
    (maybe-whitespace gs device)))

;; TD
(defmethod op-text-move-set (operands gs (device text-output-device))
  (let ((ty (second operands)))
    (op-set-text-leading (list (- ty)) gs device)
    (op-text-move operands gs device)))

;; Tm
(defmethod op-set-text-matrix (operands gs (device text-output-device)) ; FIXME general?
  (destructuring-bind (a b c d e f) operands
    (let ((new (make-matrix (list a b 0 c d 0 e f 1))))
      (setf (text-matrix device) new
	    (text-line-matrix device) new)
      (maybe-whitespace gs device))))

;; T*
(defmethod op-text-next-line (operands gs (device text-output-device))
  (op-text-move (list 0 (- (leading gs))) gs device))

;; Tj
(defmethod op-show-text (operands gs (device text-output-device))
  (let ((font (text-font gs))
	(tfs (text-font-size gs))
	(tc (character-spacing gs))
	(tw (word-spacing gs))
	(th (horizontal-scaling gs)))
    (loop for character-code across (unescape-string (first operands))
	  for w0 = (get-character-width font (char-code character-code))
	  for char = (code-char (character-code->unicode-value font character-code)) ; FIXME
	  do
	     (let* ((tx (* (+ (* w0 tfs) tc (if (char= #\Space char) tw 0)) th))
		    (old (text-matrix device))
		    (new (m* (make-matrix (list 1 0 0 0 1 0 tx 0 1)) old)))
	       (setf (text-matrix device) new
		     (previous-matrix device) new))
	     (push char (text-output device)))))

;; '
(defmethod op-move-show-text (operands gs (device text-output-device))
  (op-text-next-line nil gs device)
  (op-show-text operands gs device))

;; "
(defmethod op-move-set-show-text (operands gs (device text-output-device))
  (op-set-word-spacing (list (first operands)) gs device)
  (op-set-char-spacing (list (second operands)) gs device)
  (op-move-show-text (list (third operands)) gs device))

;; TJ
(defmethod op-show-space-text (operands gs (device text-output-device))
  (loop for operand across (first operands)
	if (stringp operand)
	  do (op-show-text (list operand) gs device)
	if (numberp operand)
	  do (let* ((tx (* (text-font-size gs)
			   (- (/ operand 1000))
			   (horizontal-scaling gs)))
		    (new (m* (make-matrix (list 1 0 0 0 1 0 tx 0 1)) (text-matrix device))))
	       (setf (text-matrix device) new)
	       (maybe-horizontal-whitespace gs device))))
	       

;; q push gs on stack

;; Q try to pop gs from stack

;; cm
(defmethod op-concat (operands gs (device text-output-device)) ; FIXME general? / before?
  #+(or)(destructuring-bind (a b c d e f) operands
    (setf (ctm gs)
	  (m* (make-matrix (list a b 0 c d 0 e f 1))
	      (ctm gs)))))

(defun maybe-horizontal-whitespace (gs device)
  (let* ((tm (text-matrix device))
	 (m (m* tm (ctm gs)))
	 (prev (previous-matrix device))
	 (dx (- (aref m 6) (aref prev 6))))
    (setf (previous-matrix device) m)
    (when (> dx 1)
	(unless (and (text-output device)
		     (white-char-p (first (text-output device))))
	  (push #\Space (text-output device))))))

(defun maybe-whitespace (gs device)
  (let* ((tm (text-matrix device))
	 (m (m* tm (ctm gs)))
	 (prev (previous-matrix device))
	 (dx (- (aref m 6) (aref prev 6)))
	 (dy (- (aref m 7) (aref prev 7))))
    (setf (previous-matrix device) m)
    (if (< dy -1)
	(unless (and (text-output device)
		     (char= #\Newline (first (text-output device))))
	  (push #\Newline (text-output device)))
	(when (> dx 1)
	  (unless (and (text-output device)
		       (white-char-p (first (text-output device))))
	    (push #\Space (text-output device))))))))

(defmethod render-page (page (device text-output-device))
  (let ((*page* page)
	(gs (make-instance 'graphics-state :ctm (make-matrix)))
	(content (group-operators (read-page-content page))))
    (dolist (entry content)
      (let* ((operator (car entry))
	     (opfunc (operator-function operator))
	     (operands (rest entry)))
	(check-operands operator operands)
	(funcall opfunc operands gs device)))
    (concatenate 'string (nreverse (text-output device)))))

;;; FILTERS

(defclass filter ()
  ())

(defgeneric decode (object filter))

(defclass ascii-hex-decode (filter)
  ())

(defmethod decode ((string string) (filter ascii-hex-decode))
  (loop with prev = 0
	with result = '()
	with count = 1
	for char across string
	for hex-value = (digit-char-p char 16)
	do (cond ((white-char-p char)
		  nil)
		 ((and hex-value (oddp count))
		  (setf prev hex-value)
		  (incf count))
		 ((and hex-value (evenp count))
		  (push (+ (* 16 prev) hex-value) result)
		  (incf count))
		 ((char= #\> char)
		  (when (evenp count)
		    (push (* 16 prev) result))
		  (return (apply 'vector (nreverse result))))
		 (t (error "Unknown char in ASCIIHEXDecode: ~C" char)))))
		  
(defclass ascii-85-decode (filter)
  ())

(defmethod decode ((string string) (filter ascii-85-decode))
  (let ((accum '())
	(count 0)
	(result '())
	(tilde-seen nil))
    (flet ((push-to-result ()
	     (let ((num (loop for i below count
			      summing (* (elt accum i) (expt 85 i)))))
	       (loop for i downfrom (- count 2) to 0
		     do (push (ldb (byte 8 (* i 8)) num) result)))))
      (loop for char across string
	    for code = (char-code char)
	    do (cond ((white-char-p char)
		      nil)
		     ((char= #\~ char)
		      (setf tilde-seen t))
		     ((and tilde-seen (char= #\> char))
		      (loop-finish))
		     ((<= #.(char-code #\!) code #.(char-code #\u))
		      (push (- code 33) accum)
		      (incf count)
		      (when (= count 5)
			(push-to-result)
			(setf count 0
			      accum '())))
		     ((char= #\z char)
		      (unless (zerop count)
			(error "Saw #\z in middle of group"))
		      (loop repeat 4
			    do (push 0 result))))
	    finally (unless (zerop count)
		      (loop until (= count 5)
			    do (push 0 accum)
			       (incf count))
		      (push-to-result))
		    (return (apply 'vector (nreverse result)))))))

;;; lzw-decode - parameters, predictor
;;;  parameters: /Predictor (integer, default 1)
;;;              /Colors (integer, default 1)
;;;              /BitsPerComponent (integer, default 8)
;;;              /Columns (integer, default 1)
;;;              /EarlyChange (integer, default 1)

;;; 9-12 bits
;;;  0-255 character
;;;  256 clear table
;;;  257 EOD
;;;  258+ table entry

;;; Predictors
;;;  1  No prediction (Default)
;;;  2  TIFF Predictor 2
;;; 10  PNG prediction, PNG None on all rows
;;; 11  PNG prediction, PNG Sub on all rows
;;; 12  PNG prediction, PNG Up on all rows
;;; 13  PNG prediction, PNG Average on all rows
;;; 14  PNG prediction, PNG Paeth on all rows
;;; 15  PNG prediction, PNG optimum ??

;;; flate-decode - parameters, predictor
;;;  parameters as for LZW, but NO /EarlyChange

;;; run-length-decode

(defclass run-length-decode (filter)
  ())

;; FIXME - test, almost certainly wrong
(defmethod decode ((string string) (filter run-length-decode))
  (let ((result '()))
    (loop with index = 0
	  for char = (char string index)
	  for code = (char-code char)
	  do (cond ((= code 128)
		    (loop-finish))
		   ((<= 0 code 127)
		    (loop repeat (1+ code)
			  for i from (1+ index)
			  for char = (char string i)
			  do (push char result)
			  finally (setf index (1+ i))))
		   (t
		    (loop with repeated-char = (char string (1+ index))
			  repeat (- 257 code)
			  do (push repeated-char result)
			  finally (incf index 2))))
	  finally (return (apply 'vector (nreverse result))))))

;;; ccitt-fax-decode - parameters
;;; /K (integer, default 0)
;;;   < 0 Group 4, pure 2D
;;;   = 0 Group 3, pure 1D
;;;   > 0 Group 3, mixed 2D, etc.
;;; /EndOfLIne (boolean, default false)
;;; /EncodedByteAlign (boolean, default false)
;;; /Columns (integer, default 1728)
;;; /Rows (integer, default 0)
;;; /EndOfBlock (boolean, default true)
;;; /BlackIs1 (boolean, default false)
;;; /DamagedRowsBeforeError (integer, default 0)

;;; jbig2-decode - parameters
;;; /JBIG2Globals (stream)

;;; dct-decode - parameters
;;; /ColorTransform (ingeger, default 1 if three components and 0 otherwise)

;;; jpx-decode (only image XObjects)

;;; crypt - parameters
;;; /Type (name - /CryptFilterDecodeParms)
;;; /Name (name - corresponding to /CF entry - table 26)

#+(or)(defclass stream-predictor ()
  ((%base-stream
    :initarg :base-stream
    :reader base-stream)
   (%predictor
    :initarg :predictor
    :reader predictor)
   (%width
    :initarg :width
    :reader pixels-per-line)))
