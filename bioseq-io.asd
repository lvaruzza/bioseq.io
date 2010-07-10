(defpackage #:bioseq-io-system
  (:use :cl :asdf))

(in-package #:bioseq-io-system)

(defsystem :bioseq-io
  :version "0.1"
  :author "Leonardo Varuzza <varuzza@gmail.com>"
  :licence "GPL"
  :serial t
  :components ((:file "packages")
	       (:file "sequilder")
	       (:file "fasta")))



