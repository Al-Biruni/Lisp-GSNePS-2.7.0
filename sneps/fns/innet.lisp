;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2007
;; Research Foundation of State University of New York

;; Version: $Id: innet.lisp,v 1.3 2007/08/21 01:54:30 mwk3 Exp $

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


(in-package :sneps)


; ==========================================================================
;
; innet
; ------
;
;       arguments     : fl - <filename> 
;
;       returns       : <nothing>
;
;       description   : "User function" to be called from SNePS environment.   
;                        It opens file fl and, if the content of file fl
;                        is a SNePS network, it inputs the network, 
;                        otherwise simply returns appropriate message.
;
;       side-effects  : It changes the entire <network> and prints a
;                       message. All previous existing contexts, nodes
;                       are erased before loadind the stored net.
;        
;
;        
;
;                                        written:  jgn 08/27/83
;                                        modified: ejm 06/01/84
;                                                  ssc 02/13/87
;                                                  njm 09/26/88
;                                                   hc 06/30/93
;                                                   hc 07/18/93
(defsnepscom innet ((file) (top) t)
  (with-open-file (inunit (user:sneps-translate file) :direction :input)
    (let* ((*package* (find-package 'snepsul))
	   (netid (read inunit)))
      (cond ((and (symbolp netid)
                  (string= (symbol-name netid) "SNePS network 2.0"))
	     ;; we've got the right one baby:
	     (resetnet t)
	     (inindices inunit)
	     (inrelations inunit)
	     (inpaths inunit)
	     (incontexts inunit)
	     (innodes inunit)
	     (insysvars inunit)
	     (format t "~%~%Network loaded from file: ~A~%" file)
	     (values))
	    ((eq netid '|SNePS network|)
	     (sneps-error
	      (format nil "~A~
                         ~%     is an old-style SNePS network file,~
                         ~%     do `(load \"sneps:sneps;fns;oinnet\")' ~
                                and use `oinnet'~
                         ~%     instead of `innet' if you want to convert it."
		      file)
	      'innet
	      'innet))
	    (t (sneps-error
		(format nil "~A is not~
                           ~%     a SNePS network file" file)
		'innet
		'innet))))))

(defun inindices (inunit)
  ;; Restores node-ID and context-ID indices:
  (dolist (p '(b m v p tm tv tp))
    (setf (get 'gennewnode p) (read inunit)))
  (setf (get 'gennewcontext 'c) (read inunit)))

(defun inrelations (inunit)
  (dotimes (i (read inunit))
    (read.r inunit)))

(defun inpaths (inunit)
  (dotimes (i (read inunit))
    (read-path.r inunit)))

(defun incontexts (inunit)
  (dotimes (i (read inunit))
    (read.ct inunit)))

(defun innodes (inunit)             ;Modified,  hi 3/28/99
  (let ((count 0))
    (dotimes (i (read inunit))
      (read.n inunit (incf count)))
    (set-node-counter count)))

;
;
;
; ==========================================================================
;
; insysvars
; ---------
;
;       arguments     : inunit - <input port> 
;
;       returns       : ignored
;
;       nonlocal-vars : The system variables (side-effected by read.sv)
;
;       description   : Calls read.sv to read S-expressions from INUNIT
;                       and sets the values of the system variables to
;                       these S-expressions.
;
;       side-effects  : read.sv sets the values of the system variables 
;
;                                          written : jgn 10/10/83
;                                          modified: ssc 02/25/87
;                                          modified: ssc 05/03/88
;                                                    hc  06/29/93
;                                                    hi  03/28/99
;
(defun insysvars (inunit)
  (let ((nodes (value.sv 'nodes)))
    (dotimes (i (read inunit))
      (read.sv inunit))
    ;If old version and *nodes is not in the file, then sort the one constructed
    ;by building the nodes.
    (unless (eql nodes (value.sv 'nodes))
      (setf (value.sv 'nodes) (sort (value.sv 'nodes) #'isless.n)))))



    
    




