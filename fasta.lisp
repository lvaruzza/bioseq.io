(defun read-fasta (name)
  (with-open-file (in name)
    (read-fasta-stream in)))


(defun read-fasta-stream (in)
  (loop for
       line = (read-line in nil nil)
       while line
       do (progn
	    (print line))))

(defun string-buffer-append (buff ch)
  (cons ch buff))

(defun valid-fasta-char (ch)
  (not (or (eq ch #\Space) (eq ch #\NewLine) (eq ch #\Tab))))

(defun fasta-sequence-append (buff ch)
  (if (valid-fasta-char ch)
      (cons ch buff)
      buff))

(defun make-fasta-sequence (header-buff seq-buff)
  (list (coerce (reverse header-buff) 'string)
	(coerce (reverse seq-buff) 'string)))

(defun read-fasta-binary (name fun)
  (with-open-file (in name :element-type 'base-char)
    (let ((buff (make-array 15 :element-type 'base-char))
	  (state 'unk)
	  (position 0)
	  (header-buff nil)
	  (old-ch #\NewLine)
	  (seq-buff nil))
      (loop 
	 for 
	   readed-bytes = (read-sequence buff in)
	   while (> readed-bytes 0)
	   do (progn 
		(loop 
		   for ch across buff
		   for skip = nil
		     do (progn
			  
			  (case ch 
			    (#\> (progn
				   (when (eq old-ch #\NewLine)
				     (unless (eq state 'unk)
				       (funcall fun (make-fasta-sequence header-buff seq-buff))
				       (setf header-buff nil)
				       (setf seq-buff nil))
				     (setf state 'header)
				     (setf skip t)
				     (format t  "new Starting sequence at ~a~%" position))))
			    (#\NewLine (when (eq state 'header)
					   (setf state 'sequence)
					   (setf skip t))))

			  (unless skip
			    (case state
				 (header (setf header-buff (string-buffer-append header-buff ch)))
				 (sequence (setf seq-buff (fasta-sequence-append seq-buff ch)))
				 ))
			       
			  (incf position)
			  (setf old-ch ch)
			  ))
		))
      (funcall fun (make-fasta-sequence header-buff seq-buff))
      position)))



  