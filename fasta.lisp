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

(defun read-fasta-sequences (name fun &key (buffer-size #.(* 10 1024 1024)))
  (let ((element-type 'base-char)) ;; (unsigned-byte 8)))
    (with-open-file (in name :element-type element-type)
      (process-fasta-stream-binary in
				   (lambda (header seq)
				     (funcall fun (make-fasta-sequence header seq)))
				   :element-type element-type :buff-size buffer-size))))


(defun process-fasta-stream-binary (in fun &key element-type buff-size)
    (let ((buff (make-array buff-size :element-type element-type))
	  (state 'unk)
	  (position 0)
	  (header-buff (make-instance 'array-builder :type element-type))
	  (old-ch #\NewLine)
	  (seq-buff (make-instance 'array-builder :type element-type)))
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



  