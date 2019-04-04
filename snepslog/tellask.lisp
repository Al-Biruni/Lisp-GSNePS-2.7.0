;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSLOG; Base: 10 -*-

;; Copyright (C) 1984--2007
;; Research Foundation of State University of New York

;; Version: $Id: tellask.lisp,v 1.7 2007/08/21 01:54:33 mwk3 Exp $

;; This file is part of SNePS.

;; $BEGIN LICENSE$

;; 
;; The contents of this file are subject to the University at
;; Buffalo Public License Version 1.0 (the "License"); you may
;; not use this file except in compliance with the License. You
;; may obtain a copy of the License at http://www.cse.buffalo.
;; edu/sneps/Downloads/ubpl.pdf.
;; 
;; Software distributed under the License is distributed on an
;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;; or implied. See the License for the specific language gov
;; erning rights and limitations under the License.
;; 
;; The Original Code is SNePS 2.7.
;; 
;; The Initial Developer of the Original Code is Research Foun
;; dation of State University of New York, on behalf of Univer
;; sity at Buffalo.
;; 
;; Portions created by the Initial Developer are Copyright (C)
;; 2007 Research Foundation of State University of New York, on
;; behalf of University at Buffalo. All Rights Reserved.
;; 
;;  
;; 
;; 


;; $END LICENSE$



;;; Tell-Ask Interface to SnePS
;;; by Stuart C. Shapiro
;;; August 12, 1997

;;; UPDATE by scs, 8/26/2004  (flj)
;;; Fixes bugs in the tell-ask interface code,
;;;    and provides ask, askifnot, askwh, and askwhnot as SNePSLOG commands.


(in-package :snepslog) 

(defun tell (string)
  "String must be valid SNePSLOG input.
   Gives the string to the SNePSLOG interpreter.
   Returns, if  possible, asserted or deduced nodes, nil otherwise."
  (let ((result nil))
    (sneps:in.environment
     :variables ((*package* (find-package :snepsul))
		 (*print-length* nil)
		 (*print-level* nil)
		 (*print-pretty* t)
		 (old-infertrace snip:*infertrace*)
		 (sneps:outunit nil))
     :functions ((surface 'snepslog:surface)
		 (slight-surface #'snepslog:slight-surface)
		 (node-intern #'snepslog:node-intern)
		 (sneps-node? #'snepslog:sneps-node?))
     :eval (progn
	     (sneps::snepslog-init "")
	     (if (string-equal (subseq string 0 4) "demo")
		 (tellDemo string)
	       (let ((sneps:outunit t)
		     (sneps:inunit t))
		 (catch 'sneps:sneps-error
		   (setf result (sneps:topsneval
				 (let ((*package* (find-package :snepslog)))
				   (snepslog-read-from-string string))))
		   t))))
     :always.do.this (progn (setq snip:*infertrace* old-infertrace)
			    (snepslog:snepslogreadoff)
			    nil)) result))

(defun ask (string &key verbose)
  "String must be a valid ask input.
   Gives that string to the SNePSLOG interpreter.
   but uses DEDUCETRUE instead of DEDUCE.
   Returns the nodes resulting from the DEDUCETRUE.
   If :VERBOSE is T, prints the results as well as returning the nodes."
  (sneps:in.environment
   :variables ((*package* (find-package :snepsul))
	       (*print-length* nil)
	       (*print-level* nil)
	       (*print-pretty* t)
	       (old-infertrace snip:*infertrace*)
	       (sneps:outunit nil))
   :functions ((surface 'snepslog:surface)
	       (slight-surface #'snepslog:slight-surface)
	       (node-intern #'snepslog:node-intern)
	       (sneps-node? #'snepslog:sneps-node?))
   :eval (progn
	   (sneps::snepslog-init "") 
	   (catch 'sneps:sneps-error
	     (let* ((sneps:outunit user:*default-output-stream*)
		    (sneps:inunit t)
		    (command
		     (let ((*package* (find-package :snepslog)))
		       (snepslog-read-from-string 
			(if (member (char string (1- (length string)))
				    '(#\. #\! #\?) :test #'char=)
			    string (concatenate 'string string "?")))))
		    (results
		     (sneps:topsneval
		      (if (eql (first command) 'snip:deduce)
			  (cons 'snip:deducetrue (rest command))
			command))))
	       (when verbose (mapc #'snepslog:snepslog-print results))
	       results)))
   :always.do.this (progn (setq snip:*infertrace* old-infertrace)
			  (snepslog:snepslogreadoff)
			  nil)))

(defun askifnot (string &key verbose)
  "String must be a valid ask input.
   Gives that string to the SNePSLOG interpreter.
   but uses DEDUCEFALSE instead of DEDUCE.
   Returns the nodes resulting from the DEDUCEFALSE.
   If :VERBOSE is T, prints the results as well as returning the nodes."
  (sneps:in.environment
   :variables ((*package* (find-package :snepsul))
	       (*print-length* nil)
	       (*print-level* nil)
	       (*print-pretty* t)
	       (old-infertrace snip:*infertrace*)
	       (sneps:outunit nil))
   :functions ((surface 'snepslog:surface)
	       (slight-surface #'snepslog:slight-surface)
	       (node-intern #'snepslog:node-intern)
	       (sneps-node? #'snepslog:sneps-node?))
   :eval (progn
	   (sneps::snepslog-init "") 
	   (catch 'sneps:sneps-error
	     (let* ((sneps:outunit user:*default-output-stream*)
		    (sneps:inunit t)
		    (command
		     (let ((*package* (find-package :snepslog)))
		       (snepslog-read-from-string
			(if (member (char string (1- (length string)))
				    '(#\. #\! #\?) :test #'char=)
			    string (concatenate 'string string "?")))))
		    (results
		     (sneps:topsneval
		      (if (eql (first command) 'snip:deduce)
			  (cons 'snip:deducefalse (rest command))
			command))))
	       (when verbose (mapc #'snepslog:snepslog-print results))
	       results)))
   :always.do.this (progn (setq snip:*infertrace* old-infertrace)
			  (snepslog:snepslogreadoff)
			  nil)))

(defun askwh (string &key verbose)
  "String must be a valid ask input.
   Gives that string to the SNePSLOG interpreter,
   but uses DEDUCEWH instead of DEDUCE.
   Returns the nodes resulting from the DEDUCEWH.
   If :VERBOSE is T, prints the results as well as returning the nodes."
  (sneps:in.environment
   :variables ((*package* (find-package :snepsul))
	       (*print-length* nil)
	       (*print-level* nil)
	       (*print-pretty* t)
	       (old-infertrace snip:*infertrace*)
	       (sneps:outunit nil))
   :functions ((surface 'snepslog:surface)
	       (slight-surface #'snepslog:slight-surface)
	       (node-intern #'snepslog:node-intern)
	       (sneps-node? #'snepslog:sneps-node?))
   :eval (progn
	   (sneps::snepslog-init "") 
	   (catch 'sneps:sneps-error
	     (let* ((sneps:outunit user:*default-output-stream*)
		    (sneps:inunit t)
		    (command
		     (let ((*package* (find-package :snepslog)))
		       (snepslog-read-from-string
			(if (member (char string (1- (length string)))
				    '(#\. #\! #\?) :test #'char=)
			    string (concatenate 'string string "?")))))
		    (results
		     (sneps:topsneval
		      (cond ((eql (first command) 'snip:deduce)
			     (cons 'snip:deducewh (rest command)))
			    ((eql (first command) 'findassert)
			     (cons 'snip:deducewh
				   (cons '(0 0)
					 (mapcar
					  #'(lambda (elt)
					      (if (and (consp elt)
						       (eql (first elt)
							    '?))
						  `($ ',(second elt))
						elt))
					  (rest command)))))
			    (t command)))))
	       (when verbose
		 (snepslog:snepslog-print results))
	       results)))
   :always.do.this (progn (setq snip:*infertrace* old-infertrace)
			  (snepslog:snepslogreadoff)
			  nil)))

(defun askwhnot (string &key verbose)
  "String must be a valid ask input.
   Gives that string to the SNePSLOG interpreter,
   but uses DEDUCEWHNOT instead of DEDUCE.
    Returns the nodes resulting from the DEDUCEWHNOT.
   If :VERBOSE is T, prints the results as well as returning the nodes."
  (sneps:in.environment
   :variables ((*package* (find-package :snepsul))
	       (*print-length* nil)
	       (*print-level* nil)
	       (*print-pretty* t)
	       (old-infertrace snip:*infertrace*)
	       (sneps:outunit nil))
   :functions ((surface 'snepslog:surface)
	       (slight-surface #'snepslog:slight-surface)
	       (node-intern #'snepslog:node-intern)
	       (sneps-node? #'snepslog:sneps-node?))
   :eval (progn
	   (sneps::snepslog-init "") 
	   (catch 'sneps:sneps-error
	     (let* ((sneps:outunit user:*default-output-stream*)
		    (sneps:inunit t)
		    (command
		     (let ((*package* (find-package :snepslog)))
		       (snepslog-read-from-string
			(if (member (char string (1- (length string)))
				    '(#\. #\! #\?) :test #'char=)
			    string (concatenate 'string string "?")))))
		    (results
		     (sneps:topsneval
		      (cond ((eql (first command) 'snip:deduce)
			     (cons 'snip:deducewhnot (rest command)))
			    ((eql (first command) 'sneps:findassert)
			     (cons 'snip:deducewhnot
				   (cons '(0 0)
					 (mapcar
					  #'(lambda (elt)
					      (if (and (consp elt)
						       (eql (first elt)
							    '?))
						  `($ ',(second elt))
						elt))
					  (rest command)))))
			    (t command)))))
	       (when verbose (snepslog:snepslog-print results))
	       results)))
   :always.do.this (progn (setq snip:*infertrace* old-infertrace)
			  (snepslog:snepslogreadoff)
			  nil)))

(defun tellDemo (democmd)
  "Executes the SNePSLOG demo command."
  ;; For use by tell when it's called as
  ;;      (tell "demo [<file path>] [t | b | bv | a | av | n] [.]")
  ;; Should be callable from any Lisp code, or from the SNePS GUI.
  (sneps:snepslog :inunit (make-echo-stream
			   (make-string-input-stream
			    (format nil "~%~A~%lisp" democmd))
			   *standard-output*)
		  :hello-text "")
  (values))
