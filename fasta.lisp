(in-package :bioseq.io)

(defun valid-fasta-char (ch)
  (not (or (eq ch #\Space) (eq ch #\NewLine) (eq ch #\Tab))))

(defun fasta-sequence-append (buff ch)
  (if (valid-fasta-char ch)
      (ar-append buff ch)
      buff))

(defun make-fasta-sequence (header-buff seq-buff)
  (list (copy-result header-buff)
	(copy-result seq-buff)))

(defconstant +read-fasta-element-type+ 'base-char)

(defun read-fasta-sequences (name fun &key (buffer-size #.(* 10 1024 1024)))
  (with-open-file (in name :element-type +read-fasta-element-type+)
    (process-fasta-stream in
			  (lambda (header seq)
			    (funcall fun (make-fasta-sequence header seq)))
			  :seq-element-type 'base-char :buff-size buffer-size)))


(defun process-fasta-stream (in fun &key seq-element-type buff-size)
    (let ((buff (make-array buff-size :element-type +read-fasta-element-type+))
	  (state 'unk)
	  (position 0)
	  (header-buff (make-instance 'array-builder :type +read-fasta-element-type+))
	  (old-ch #\NewLine)
	  (seq-buff (make-instance 'array-builder :type seq-element-type)))
      (loop 
	 for 
	   readed-bytes = (read-sequence buff in)
	   while (> readed-bytes 0)
	   do (progn 
		(loop 
		   for ii from 0 to (1- readed-bytes)
		   for ch = (aref buff ii)
		   for skip = nil
		     do (progn
			  
			  (case ch 
			    (#\> (progn
				   (when (eq old-ch #\NewLine)
				     (unless (eq state 'unk)
				       (funcall fun header-buff seq-buff)
				       (reset-buffer header-buff)
				       (reset-buffer seq-buff))
				     (setf state 'header)
				     (setf skip t)
				     )))
			    (#\NewLine (when (eq state 'header)
					   (setf state 'sequence)
					   (setf skip t))))

			  (unless skip
			    (case state
				 (header (ar-append header-buff ch))
				 (sequence (fasta-sequence-append seq-buff ch))
				 ))
			       
			  (incf position)
			  (setf old-ch ch)
			  ))
		))
      (funcall fun header-buff seq-buff)
      position))



  