(declaim (optimize (speed 3) (safety 1)))

(defconstant +initial-string-bulter-buffer+ 128)

(defun calc-new-buff-size (size)
  (declare (type fixnum size))
  (the fixnum (* 2 (the fixnum size))))

(defclass array-builder ()
  ((pos :initform 0)
   (type :initform t :initarg :type )
   (buff)))

(defmethod initialize-instance :after ((arb 
array-builder) &key)
  (with-slots (buff pos type) arb
    (setf buff (make-array +initial-string-bulter-buffer+ 
			   :element-type type
			   :adjustable t))))


(defmethod ar-append ((ab array-builder) thing)
  (with-slots (buff pos) ab
    (setf (aref buff pos) thing)
    (incf pos)
    (when (>= pos (array-dimension buff 0))
      (adjust-array buff (calc-new-buff-size pos)))))


(defmethod copy-result ((arb array-builder) result)
  (with-slots (buff pos) arb
    (loop for i from 0 to (1- pos)
	 do (setf (aref result i) (aref buff i)))))

(defmethod make-result-copy ((arb array-builder))
  (with-slots (buff pos) arb
    (subseq buff 0 pos)))

(defmethod reset-buffer ((arb array-builder))
  (with-slots (buff pos) arb
    (setf pos 0)))

(defmethod copy-and-reset ((arb array-builder))
  (with-slots (pos) arb
    (let ((result (make-array pos)))
      (copy-result arb result)
      (reset-buffer arb)
      result)))

(defmethod copy-and-reset2 ((arb array-builder) result)
  (with-slots (pos) arb
    (when (/= (array-dimension result 0) pos)
      (adjust-array result pos))
    (copy-result arb result)
    (reset-buffer arb)
    result))


(defmethod dump ((arb array-builder))
  (with-slots (buff pos) arb
    (print buff)
    (print pos)))

(defun test1 ()
  (let ((ar (make-instance 'array-builder)))
    (ar-append ar #\A)
    (ar-append ar #\B)
    (print (make-result-copy ar))))

(defun test2 ()
  (let ((ar (make-instance 'array-builder)))
    (ar-append ar #\A)
    (ar-append ar #\B)
    (print (copy-and-reset ar))
    (ar-append ar #\C)
    (ar-append ar #\D)
    (print (copy-and-reset ar))
    nil))

(defun test-long-append () 
  (let ((ar (make-instance 'array-builder :type 'fixnum)))
    (loop for i from 1 to 10001
	 do (ar-append ar i))
    (print (aref (make-result-copy ar)  10000))))
    
(defun performance-test ()
  (let* ((ar (make-instance 'array-builder :type 'fixnum))
	 (sum 0)
	 (a (make-array 10 :element-type 'fixnum :adjustable t))
	 (n #.(* 40 1000 1000))
	 (sum2 (/ (* n (+ 1 n)) 2)))
    (declare (type integer sum n))
    (loop for i from 1 to n
       do (progn
	    (ar-append ar i)
	    (when (= (mod i 10) 0)
	      (copy-and-reset2 ar a)
	      (loop for x across a
		 do (setf sum (+ sum (the fixnum x)))))))
    (print (list sum sum2))))
    
		